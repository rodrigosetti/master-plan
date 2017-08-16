{-|
Module      : MasterPlan.Data
Description : Types for defining project and project systems
Copyright   : (c) Rodrigo Setti, 2017
License     : MIT
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}
module MasterPlan.Data ( ProjectExpr(..)
                       , ProjectProperties(..)
                       , ProjectSystem(..)
                       , Binding(..)
                       , ProjectKey(..)
                       , ProjAttribute(..)
                       , Trust(..)
                       , Cost(..)
                       , Progress(..)
                       , defaultProjectProps
                       , defaultCost
                       , defaultTrust
                       , defaultProgress
                       , defaultTaskProj
                       , bindingTitle
                       , cost
                       , progress
                       , trust
                       , simplify
                       , simplifyProj
                       , prioritizeSys
                       , prioritizeProj ) where

import           Data.Generics
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import           Data.String        (IsString)

-- * Types

newtype Trust = Trust { getTrust :: Float }
  deriving (Show, Eq, Data, Typeable, Ord, Num, Real, RealFrac, Fractional)
newtype Cost = Cost { getCost :: Float }
  deriving (Show, Eq, Data, Typeable, Ord, Num, Real, RealFrac, Fractional)
newtype Progress = Progress { getProgress :: Float }
  deriving (Show, Eq, Data, Typeable, Ord, Num, Real, RealFrac, Fractional)

newtype ProjectKey = ProjectKey { getProjectKey :: String }
  deriving (Show, Eq, Data, Typeable, Ord, IsString)

-- |Structure of a project expression
data ProjectExpr = Sum (NE.NonEmpty ProjectExpr)
             | Product (NE.NonEmpty ProjectExpr)
             | Sequence (NE.NonEmpty ProjectExpr)
             | Reference ProjectKey
            deriving (Eq, Show, Data, Typeable)

-- |A binding of a name can refer to an expression. If there are no
-- associated expressions (i.e. equation) then it can have task-level
-- properties
data Binding = BindingAtomic ProjectProperties Cost Trust Progress
                    | BindingExpr ProjectProperties ProjectExpr
                   deriving (Eq, Show, Data, Typeable)

-- |Any binding (with a name) may have associated properties
data ProjectProperties = ProjectProperties { title       :: String
                                           , description :: Maybe String
                                           , url         :: Maybe String
                                           , owner       :: Maybe String
                                           } deriving (Eq, Show, Data, Typeable)

data ProjAttribute = PTitle | PDescription | PUrl | POwner | PCost | PTrust | PProgress
  deriving (Eq, Enum, Bounded)

instance Show ProjAttribute where
  show PTitle       = "title"
  show PDescription = "description"
  show PUrl         = "url"
  show POwner       = "owner"
  show PCost        = "cost"
  show PTrust       = "trust"
  show PProgress    = "progress"

-- |A project system defines the bindins (mapping from names to expressions or tasks)
-- and properties, which can be associated to any binding
newtype ProjectSystem = ProjectSystem { bindings :: M.Map ProjectKey Binding }
                          deriving (Eq, Show, Data, Typeable)

defaultProjectProps ∷ ProjectProperties
defaultProjectProps = ProjectProperties { title = "?"
                                        , description = Nothing
                                        , url = Nothing
                                        , owner = Nothing }

defaultCost ∷ Cost
defaultCost = 0

defaultTrust ∷ Trust
defaultTrust = 1

defaultProgress ∷ Progress
defaultProgress = 0

defaultTaskProj ∷ ProjectProperties → Binding
defaultTaskProj pr = BindingAtomic pr defaultCost defaultTrust defaultProgress

bindingTitle ∷ Binding → String
bindingTitle (BindingAtomic ProjectProperties { title=t} _ _ _) = t
bindingTitle (BindingExpr ProjectProperties { title=t} _)       = t

-- | Expected cost
cost ∷ ProjectSystem → ProjectExpr → Cost
cost sys (Reference n) =
  case M.lookup n (bindings sys) of
    Just (BindingAtomic _ (Cost c) _ (Progress p)) -> Cost $ c * (1 - p) -- cost is weighted by remaining progress
    Just (BindingExpr _ p)                         -> cost sys p -- TODO:0 avoid cyclic
    Nothing                                        -> defaultCost -- mentioned but no props neither task defined
cost sys (Sequence ps) = costConjunction sys ps
cost sys (Product ps) = costConjunction sys ps
cost sys (Sum ps) =
   Cost $ sum $ map (\x -> (1 - snd x) * fst x) $ zip costs accTrusts
 where
   costs = NE.toList $ (getCost . cost sys) <$> ps
   accTrusts = NE.toList $ NE.scanl (\a b -> a + b*(1-a)) 0 $ (getTrust . trust sys) <$> ps

costConjunction ∷ ProjectSystem → NE.NonEmpty ProjectExpr → Cost
costConjunction sys ps =
   Cost $ sum $ zipWith (*) costs accTrusts
  where
    costs = NE.toList $ (getCost . cost sys) <$> ps
    accTrusts = NE.toList $ product <$> NE.inits ((getTrust . trust sys) <$> ps)

-- | Expected probability of succeeding
trust ∷ ProjectSystem → ProjectExpr → Trust
trust sys (Reference n) =
  case M.lookup n (bindings sys) of
    Just (BindingAtomic _ _ (Trust t) (Progress p)) -> Trust $ p + t * (1-p)
    Just (BindingExpr _ p)                          -> trust sys p -- TODO:10 avoid cyclic
    Nothing                                         -> defaultTrust -- mentioned but no props neither task defined
trust sys (Sequence ps) = trustConjunction sys ps
trust sys (Product ps) = trustConjunction sys ps
trust sys (Sum ps) =
  Trust $ foldl (\a b -> a + b*(1-a)) 0 $ (getTrust . trust sys) <$> ps

trustConjunction ∷ ProjectSystem → NE.NonEmpty ProjectExpr → Trust
trustConjunction sys ps = Trust $ product $ (getTrust . trust sys) <$> ps

progress ∷ ProjectSystem → ProjectExpr → Progress
progress sys (Reference n) =
  case M.lookup n (bindings sys) of
    Just (BindingAtomic _ _ _ p) -> p
    Just (BindingExpr _ p)       -> progress sys p -- TODO:20 avoid cyclic
    Nothing                      -> defaultProgress -- mentioned but no props neither task defined
progress sys (Sequence ps)   = progressConjunction sys ps
progress sys (Product ps)    = progressConjunction sys ps
progress sys (Sum ps)        = maximum $ progress sys <$> ps

progressConjunction ∷ ProjectSystem → NE.NonEmpty ProjectExpr → Progress
progressConjunction sys ps = sum (progress sys <$> ps) / fromIntegral (length ps)

-- |Simplify a project binding structure
simplify ∷ ProjectSystem → ProjectSystem
simplify = everywhere (mkT simplifyProj)

-- |Simplify a project expression structure
--  1) transform singleton collections into it's only child
--  2) flatten same constructor of the collection
simplifyProj ∷ ProjectExpr → ProjectExpr
simplifyProj (Sum (p :| []))      = simplifyProj p
simplifyProj (Product (p :| []))  = simplifyProj p
simplifyProj (Sequence (p :| [])) = simplifyProj p
simplifyProj (Sum ps) =
    Sum $ (reduce . simplifyProj) =<< ps
  where
    reduce (Sum ps') = reduce =<< ps'
    reduce p         = [simplifyProj p]
simplifyProj (Product ps) =
    Product $ (reduce . simplifyProj) =<< ps
  where
    reduce (Product ps') = reduce =<< ps'
    reduce p             = [simplifyProj p]
simplifyProj (Sequence ps) =
    Sequence $ (reduce . simplifyProj) =<< ps
  where
    reduce (Sequence ps') = reduce =<< ps'
    reduce p              = [simplifyProj p]
simplifyProj p@Reference {}     = p

-- |Sort projects in the system order that minimizes cost
prioritizeSys ∷ ProjectSystem → ProjectSystem
prioritizeSys sys = everywhere (mkT $ prioritizeProj sys) sys

-- |Sort project in order that minimizes cost
prioritizeProj ∷ ProjectSystem → ProjectExpr → ProjectExpr
prioritizeProj sys (Sum ps)      =
  let f p = getCost (cost sys' p) / getTrust (trust sys' p)
      sys' = prioritizeSys sys
  in Sum $ NE.sortWith (nanToInf . f) $ prioritizeProj sys' <$> ps
prioritizeProj sys (Product ps)  =
  let f p = getCost (cost sys' p) / (1 - getTrust (trust sys' p))
      sys' = prioritizeSys sys
  in Product $ NE.sortWith (nanToInf . f) $ prioritizeProj sys' <$> ps
prioritizeProj sys (Sequence ps)  =
  Sequence $ prioritizeProj sys <$> ps
prioritizeProj _   p             = p

-- |Helper function to transform any Nan (not a number) to positive infinity
nanToInf :: RealFloat a => a -> a
nanToInf x = if isNaN x then 1/0 else x
