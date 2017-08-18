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
module MasterPlan.Data ( ProjectExpr
                       , ProjectKey
                       , Project(..)
                       , ProjectProperties(..)
                       , ProjAttribute(..)
                       , Trust(..)
                       , Cost(..)
                       , Progress(..)
                       , defaultProjectProps
                       , defaultCost
                       , defaultTrust
                       , defaultProgress
                       , defaultAtomic
                       , properties
                       , cost
                       , progress
                       , trust
                       , simplify
                       , subprojects
                       , prioritize ) where

import           Data.Generics
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import           Data.Void          (Void)

-- * Types

-- |When using to reference projects by name
type ProjectKey = String

newtype Trust = Trust { getTrust :: Float }
  deriving (Show, Eq, Data, Typeable, Ord, Num, Real, RealFrac, Fractional)
newtype Cost = Cost { getCost :: Float }
  deriving (Show, Eq, Data, Typeable, Ord, Num, Real, RealFrac, Fractional)
newtype Progress = Progress { getProgress :: Float }
  deriving (Show, Eq, Data, Typeable, Ord, Num, Real, RealFrac, Fractional)

-- |Structure of a project expression
data Project e = Sum ProjectProperties (NE.NonEmpty (Project e))
               | Product ProjectProperties (NE.NonEmpty (Project e))
               | Sequence ProjectProperties (NE.NonEmpty (Project e))
               | Atomic ProjectProperties Cost Trust Progress
               | Annotated e
              deriving (Eq, Show, Data, Typeable)

type ProjectExpr = Project Void

-- |Any binding (with a name) may have associated properties
data ProjectProperties = ProjectProperties { title       :: Maybe String
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

defaultProjectProps ∷ ProjectProperties
defaultProjectProps = ProjectProperties { title = Nothing
                                        , description = Nothing
                                        , url = Nothing
                                        , owner = Nothing }

defaultCost ∷ Cost
defaultCost = 0

defaultTrust ∷ Trust
defaultTrust = 1

defaultProgress ∷ Progress
defaultProgress = 0

defaultAtomic :: Project a
defaultAtomic = Atomic defaultProjectProps defaultCost defaultTrust defaultProgress

-- | Expected cost
cost ∷ ProjectExpr → Cost
cost (Atomic _ (Cost c) _ (Progress p)) = Cost $ c * (1 - p) -- cost is weighted by remaining progress
cost (Sequence _ ps) = costConjunction ps
cost (Product _ ps) = costConjunction ps
cost (Sum _ ps) =
   Cost $ sum $ map (\x -> (1 - snd x) * fst x) $ zip costs accTrusts
 where
   costs = NE.toList $ (getCost . cost) <$> ps
   accTrusts = NE.toList $ NE.scanl (\a b -> a + b*(1-a)) 0 $ (getTrust . trust) <$> ps
cost (Annotated _) = undefined

costConjunction ∷ NE.NonEmpty ProjectExpr → Cost
costConjunction ps =
   Cost $ sum $ zipWith (*) costs accTrusts
  where
    costs = NE.toList $ (getCost . cost) <$> ps
    accTrusts = NE.toList $ product <$> NE.inits ((getTrust . trust) <$> ps)

-- | Expected probability of succeeding
trust ∷ ProjectExpr → Trust
trust (Atomic _ _ (Trust t) (Progress p)) = Trust $ p + t * (1-p) -- reduced by progress
trust (Sequence _ ps) = trustConjunction ps
trust (Product _ ps) = trustConjunction ps
trust (Sum _ ps) =
  Trust $ foldl (\a b -> a + b*(1-a)) 0 $ (getTrust . trust) <$> ps
trust (Annotated _) = undefined

trustConjunction ∷ NE.NonEmpty ProjectExpr → Trust
trustConjunction ps = Trust $ product $ (getTrust . trust) <$> ps

subprojects :: Project a -> [Project a]
subprojects (Sequence _ ps) = NE.toList ps
subprojects (Product _ ps)  = NE.toList ps
subprojects (Sum _ ps)      = NE.toList ps
subprojects _               = []


properties :: Project a -> Maybe ProjectProperties
properties (Annotated _)        = Nothing
properties (Product props _)    = Just props
properties (Sequence props _)   = Just props
properties (Sum props _)        = Just props
properties (Atomic props _ _ _) = Just props

progress ∷ ProjectExpr → Progress
progress (Atomic _ _ _ p) = p
progress (Sequence _ ps)  = progressConjunction ps
progress (Product _ ps)   = progressConjunction ps
progress (Sum _ ps)       = maximum $ progress <$> ps
progress (Annotated _)    = undefined

progressConjunction ∷ NE.NonEmpty ProjectExpr → Progress
progressConjunction ps = sum (progress <$> ps) / fromIntegral (length ps)

-- |Simplify a project expression structure
--  1) transform singleton collections into it's only child
--  2) flatten same constructor of the collection
simplify ∷ Project a → Project a
simplify (Sum _ (p :| []))      = simplify p
simplify (Product _ (p :| []))  = simplify p
simplify (Sequence _ (p :| [])) = simplify p
simplify (Sum r ps) =
    Sum r $ (reduce . simplify) =<< ps
  where
    reduce (Sum _ ps') = reduce =<< ps'
    reduce p           = [simplify p]
simplify (Product r ps) =
    Product r $ (reduce . simplify) =<< ps
  where
    reduce (Product _ ps') = reduce =<< ps'
    reduce p               = [simplify p]
simplify (Sequence r ps) =
    Sequence r $ (reduce . simplify) =<< ps
  where
    reduce (Sequence _ ps') = reduce =<< ps'
    reduce p                = [simplify p]
simplify p = p

-- |Sort project in order that minimizes cost
prioritize ∷ ProjectExpr → ProjectExpr
prioritize (Sum r ps)      =
  let f p = getCost (cost p) / getTrust (trust p)
  in Sum r $ NE.sortWith (nanToInf . f) $ prioritize <$> ps
prioritize (Product r ps)  =
  let f p = getCost (cost p) / (1 - getTrust (trust p))
  in Product r $ NE.sortWith (nanToInf . f) $ prioritize <$> ps
prioritize (Sequence r ps)  =
  Sequence r $ prioritize <$> ps
prioritize p        = p

-- |Helper function to transform any Nan (not a number) to positive infinity
nanToInf :: RealFloat a => a -> a
nanToInf x = if isNaN x then 1/0 else x
