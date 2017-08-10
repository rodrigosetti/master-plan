{-|
Module      : MasterPlan.Data
Description : Types for defining project and project systems
Copyright   : (c) Rodrigo Setti, 2017
License     : MIT
Maintainer  : rodrigosetti@email.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE UnicodeSyntax      #-}
module MasterPlan.Data ( Project(..)
                       , ProjectProperties(..)
                       , ProjectSystem(..)
                       , ProjectBinding(..)
                       , ProjectKey
                       , ProjProperty(..)
                       , Trust
                       , Cost
                       , Progress
                       , rootKey
                       , defaultProjectProps
                       , defaultTaskProj
                       , cost
                       , progress
                       , trust
                       , simplify
                       , simplifyProj
                       , optimizeSys
                       , optimizeProj
                       , printStructure) where

import           Control.Monad.Writer
import           Data.Generics
import           Data.List.NonEmpty   (NonEmpty ((:|)))
import qualified Data.List.NonEmpty   as NE
import qualified Data.Map             as M
import           Data.Semigroup       (sconcat)

-- * Types

type Trust = Float
type Cost = Float
type Progress = Float
type ProjectKey = String

-- |Structure of a project expression
data Project = SumProj (NE.NonEmpty Project)
             | ProductProj (NE.NonEmpty Project)
             | SequenceProj (NE.NonEmpty Project)
             | RefProj ProjectKey
            deriving (Eq, Show, Data, Typeable)

-- |A binding of a name can refer to an expression. If there are no
-- associated expressions (i.e. equation) then it can have task-level
-- properties
data ProjectBinding = TaskProj ProjectProperties Cost Trust Progress
                    | ExpressionProj ProjectProperties Project
                    | UnconsolidatedProj ProjectProperties
                   deriving (Eq, Show, Data, Typeable)

-- |Any binding (with a name) may have associated properties
data ProjectProperties = ProjectProperties { title       :: String
                                           , description :: Maybe String
                                           , url         :: Maybe String
                                           , owner       :: Maybe String
                                           } deriving (Eq, Show, Data, Typeable)

data ProjProperty = PTitle | PDescription | PUrl | POwner | PCost | PTrust | PProgress
  deriving (Eq, Enum, Bounded)

instance Show ProjProperty where
  show PTitle       = "title"
  show PDescription = "description"
  show PUrl         = "url"
  show POwner       = "owner"
  show PCost        = "cost"
  show PTrust       = "trust"
  show PProgress    = "progress"

-- |A project system defines the bindins (mapping from names to expressions or tasks)
-- and properties, which can be associated to any binding
newtype ProjectSystem = ProjectSystem { bindings :: M.Map ProjectKey ProjectBinding }
                          deriving (Eq, Show, Data, Typeable)

rootKey ∷ ProjectKey
rootKey = "root"

defaultProjectProps ∷ ProjectProperties
defaultProjectProps = ProjectProperties { title = rootKey
                                        , description = Nothing
                                        , url = Nothing
                                        , owner = Nothing }

defaultTaskProj ∷ ProjectProperties → ProjectBinding
defaultTaskProj pr = TaskProj pr 0 1 0

-- | Expected cost
cost ∷ ProjectSystem → Project → Cost
cost sys (RefProj n) =
  case M.lookup n (bindings sys) of
    Just (TaskProj _ c _ p)     -> c * (1-p) -- cost is weighted by remaining progress
    Just (ExpressionProj _ p)   -> cost sys p -- TODO: avoid cyclic
    Just (UnconsolidatedProj _) -> 0 -- default
    Nothing                     -> 0 -- should not happen
cost sys (SequenceProj ps) = costConjunction sys ps
cost sys (ProductProj ps) = costConjunction sys ps
cost sys (SumProj ps) =
   sum $ map (\x -> (1 - snd x) * fst x) $ zip costs accTrusts
 where
   accTrusts = NE.toList $ NE.scanl (\a b -> a + b*(1-a)) 0 $ NE.map (trust sys) ps
   costs = NE.toList $ NE.map (cost sys) ps

costConjunction ∷ ProjectSystem → NE.NonEmpty Project → Cost
costConjunction sys ps =
   sum $ zipWith (*) costs accTrusts
  where
    costs = NE.toList $ NE.map (cost sys) ps
    accTrusts = NE.toList $ NE.map product $ NE.inits $ NE.map (trust sys) ps

-- | Expected probability of succeeding
trust ∷ ProjectSystem → Project → Trust
trust sys (RefProj n) =
  case M.lookup n (bindings sys) of
    Just (TaskProj _ _ t p)     -> p + t * (1-p)
    Just (ExpressionProj _ p)   -> trust sys p -- TODO: avoid cyclic
    Just (UnconsolidatedProj _) -> 1 -- default
    Nothing                     -> 0 -- should not happen
trust sys (SequenceProj ps) = trustConjunction sys ps
trust sys (ProductProj ps) = trustConjunction sys ps
trust sys (SumProj ps) =
  foldl (\a b -> a + b*(1-a)) 0 $ NE.map (trust sys) ps

trustConjunction ∷ ProjectSystem → NE.NonEmpty Project → Trust
trustConjunction sys ps = product $ NE.map (trust sys) ps

progress ∷ ProjectSystem → Project → Progress
progress sys (RefProj n) =
  case M.lookup n (bindings sys) of
    Just (TaskProj _ _ _ p)     -> p
    Just (ExpressionProj _ p)   -> progress sys p -- TODO: avoid cyclic
    Just (UnconsolidatedProj _) -> 0 -- default
    Nothing                     -> 0 -- should not happen
progress sys (SequenceProj ps)   = progressConjunction sys ps
progress sys (ProductProj ps)    = progressConjunction sys ps
progress sys (SumProj ps)        = maximum $ NE.map (progress sys) ps

progressConjunction ∷ ProjectSystem → NE.NonEmpty Project → Progress
progressConjunction sys ps = sum (NE.map (progress sys) ps) / fromIntegral (length ps)

-- |Simplify a project binding structure
simplify ∷ ProjectSystem → ProjectSystem
simplify = everywhere (mkT simplifyProj)

-- |Helper function: concatMap for NonEmpty
neConcatMap ∷ (a → NE.NonEmpty b) → NE.NonEmpty a → NE.NonEmpty b
neConcatMap f = sconcat . NE.map f

-- |Simplify a project expression structure
--  1) transform singleton collections into it's only child
--  2) flatten same constructor of the collection
simplifyProj ∷ Project → Project
simplifyProj (SumProj (p :| []))      = simplifyProj p
simplifyProj (ProductProj (p :| []))  = simplifyProj p
simplifyProj (SequenceProj (p :| [])) = simplifyProj p
simplifyProj (SumProj ps) =
    SumProj $ neConcatMap (reduce . simplifyProj) ps
  where
    reduce (SumProj ps') = neConcatMap reduce ps'
    reduce p             = [simplifyProj p]
simplifyProj (ProductProj ps) =
    ProductProj $ neConcatMap (reduce . simplifyProj) ps
  where
    reduce (ProductProj ps') = neConcatMap reduce ps'
    reduce p                 = [simplifyProj p]
simplifyProj (SequenceProj ps) =
    SequenceProj $ neConcatMap (reduce . simplifyProj) ps
  where
    reduce (SequenceProj ps') = neConcatMap reduce ps'
    reduce p                  = [simplifyProj p]
simplifyProj p@RefProj {}     = p

optimizeSys ∷ ProjectSystem → ProjectSystem
optimizeSys sys = everywhere (mkT $ optimizeProj sys) sys

-- Sort project in order that minimizes cost
optimizeProj ∷ ProjectSystem → Project → Project
optimizeProj sys (SumProj ps)      =
  let f p = cost sys p / trust sys p
  in SumProj $ NE.sortWith f $ NE.map (optimizeProj sys) ps
optimizeProj sys (ProductProj ps)  =
  let f p = cost sys p / (1 - trust sys p)
  in ProductProj $ NE.sortWith f $ NE.map (optimizeProj sys) ps
optimizeProj _   p                 = p

-- |Debugging
printStructure ∷ Project → String
printStructure = execWriter . print' 0
   where
     ident ∷ Int → Writer String ()
     ident il = replicateM_ il $ tell " |"

     print' ∷ Int → Project → Writer String ()
     print' il (RefProj n)  = ident il >> tell ("-" ++ n ++ "\n")
     print' il (SumProj ps) = ident il >> tell "-+\n" >> forM_ ps (print' $ il+1)
     print' il (SequenceProj ps) = ident il >> tell "->\n" >> forM_ ps (print' $ il+1)
     print' il (ProductProj ps) = ident il >> tell "-*\n" >> forM_ ps (print' $ il+1)
