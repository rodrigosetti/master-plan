{-|
Module      : MasterPlan.Data
Description : Types for defining project and project systems
Copyright   : (c) Rodrigo Setti, 2017
License     : MIT
Maintainer  : rodrigosetti@email.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UnicodeSyntax   #-}
module MasterPlan.Data ( Status(..)
                       , Project(..)
                       , ProjectProperties(..)
                       , ProjectSystem(..)
                       , ProjectBinding(..)
                       , ProjectKey
                       , Trust
                       , Cost
                       , Progress
                       , defaultProjectProps
                       , defaultTaskProj
                       , cost
                       , status
                       , progress
                       , trust
                       , isClosed
                       , isOpen
                       , simplify
                       , simplifyProj) where

import           Data.Foldable      (asum)
import           Data.List          (find)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import           Data.Maybe         (fromMaybe)
import           Data.Semigroup     (sconcat)

type Trust = Float
type Cost = Float
type Progress = Float
type ProjectKey = String

data Status = Ready | Blocked | Progress | Done | Cancelled
          deriving (Eq, Show)

-- |Structure of a project expression
data Project = SumProj (NE.NonEmpty Project) |
               ProductProj (NE.NonEmpty Project) |
               SequenceProj (NE.NonEmpty Project) |
               RefProj ProjectKey
                deriving (Eq, Show)

-- |A binding of a name can refer to an expression. If there are no
-- associated expressions (i.e. equation) then it can have task-level
-- properties
data ProjectBinding = TaskProj { props            :: ProjectProperties
                               , reportedCost     :: Cost
                               , reportedTrust    :: Trust
                               , reportedStatus   :: Status
                               , reportedProgress :: Progress
                               } |
                      ExpressionProj { props      :: ProjectProperties
                                     , expression :: Project
                                     } |
                      UnconsolidatedProj { props :: ProjectProperties }
                         deriving (Eq, Show)

-- |Any binding (with a name) may have associated properties
data ProjectProperties = ProjectProperties { title       :: String
                                           , description :: Maybe String
                                           , url         :: Maybe String
                                           , owner       :: Maybe String
                                           } deriving (Eq, Show)

-- |A project system defines the bindins (mapping from names to expressions or tasks)
-- and properties, which can be associated to any binding
newtype ProjectSystem = ProjectSystem { bindings :: M.Map ProjectKey ProjectBinding }
                          deriving (Eq, Show)

defaultProjectProps ∷ ProjectProperties
defaultProjectProps = ProjectProperties { title = "root"
                                        , description = Nothing
                                        , url = Nothing
                                        , owner = Nothing }

defaultTaskProj ∷ ProjectBinding
defaultTaskProj = TaskProj { props = defaultProjectProps
                           , reportedCost = 0
                           , reportedTrust = 1
                           , reportedProgress = 0
                           , reportedStatus = Ready }

isOpen ∷ ProjectSystem → Project → Bool
isOpen sys p = status sys p `elem` ([Progress, Ready, Blocked] :: [Status])

isClosed ∷ ProjectSystem → Project → Bool
isClosed sys p = not $ isOpen sys p

-- | Expected cost
cost ∷ ProjectSystem → Project → Cost
cost sys p | isClosed sys p = 0
cost sys (RefProj n) =
  case M.lookup n (bindings sys) of
    Just TaskProj { reportedCost=c }    -> c
    Just ExpressionProj { expression=p} -> cost sys p -- TODO: avoid cyclic
    Just (UnconsolidatedProj _)         → 0 -- default
    Nothing                             -> 0 -- should not happen
cost sys (SequenceProj ps) = costConjunction sys ps
cost sys (ProductProj ps) = costConjunction sys ps
cost sys (SumProj ps) =
   sum $ map (\x -> (1 - snd x) * fst x) $ zip costs accTrusts
 where
   accTrusts = scanl (\a b -> a + b*(1-a)) 0 $ map (trust sys) opens
   costs = map (cost sys) opens
   opens = NE.filter (isOpen sys) ps

costConjunction ∷ ProjectSystem → NE.NonEmpty Project → Cost
costConjunction sys ps =
   sum $ zipWith (*) costs accTrusts
  where
    costs = NE.toList $ NE.map (cost sys) ps
    accTrusts = NE.toList $ NE.map product $ NE.inits $ NE.map (trust sys) ps

-- | Expected trust probability
trust ∷ ProjectSystem → Project → Trust
trust sys p | isClosed sys p = 1
trust sys (RefProj n) =
  case M.lookup n (bindings sys) of
    Just TaskProj { reportedTrust=t }   -> t
    Just ExpressionProj { expression=p} -> trust sys p -- TODO: avoid cyclic
    Just (UnconsolidatedProj _)         → 1 -- default
    Nothing                             -> 0 -- should not happen
trust sys (SequenceProj ps) = trustConjunction sys ps
trust sys (ProductProj ps) = trustConjunction sys ps
trust sys (SumProj ps) =
  if null opens then 1 else accTrusts
 where
  accTrusts = foldl (\a b -> a + b*(1-a)) 0 $ map (trust sys) opens
  opens = NE.filter (isOpen sys) ps

trustConjunction ∷ ProjectSystem → NE.NonEmpty Project → Trust
trustConjunction sys ps = product $ NE.map (trust sys) ps

progress ∷ProjectSystem → Project → Progress
progress sys p | isClosed sys p = 1
progress sys (RefProj n) =
  case M.lookup n (bindings sys) of
    Just TaskProj { reportedProgress=p } -> p
    Just ExpressionProj { expression=p}  -> progress sys p -- TODO: avoid cyclic
    Just (UnconsolidatedProj _)          → 0 -- default
    Nothing                              -> 0 -- should not happen
progress sys (SequenceProj ps)   = progressConjunction sys ps
progress sys (ProductProj ps)    = progressConjunction sys ps
progress sys (SumProj ps)        = maximum $ NE.map (progress sys) ps

progressConjunction ∷ ProjectSystem → NE.NonEmpty Project → Progress
progressConjunction sys ps =
   let opens = NE.filter (isOpen sys) ps
    in if null opens
      then 1
      else sum (map (progress sys) opens) / fromIntegral (length opens)

status ∷ ProjectSystem → Project → Status
status sys (RefProj n) =
  case M.lookup n (bindings sys) of
    Just TaskProj { reportedProgress=p, reportedStatus=s }  -> if p>=1 then Done else s
    Just ExpressionProj { expression=p}  -> status sys p -- TODO: avoid cyclic
    Just (UnconsolidatedProj _)         → Ready -- default
    Nothing                                        -> Cancelled -- should not happen
status sys (SequenceProj ps) =
  let rest = NE.dropWhile (isClosed sys) ps
  in case rest of (p : _) -> status sys p
                  []      -> Done
status sys (ProductProj ps) =
  statusPriority [Progress, Ready, Blocked, Cancelled, Done] sys ps
status sys (SumProj ps) =
  statusPriority [Done, Progress, Ready, Blocked, Cancelled] sys ps

statusPriority ∷ [Status] → ProjectSystem → NE.NonEmpty Project → Status
statusPriority priority sys ps =
  let ss = NE.map (status sys) ps
   in fromMaybe Done $ asum $ map (\x -> find (x ==) ss) priority

-- |Simplify a project binding structure
simplify ∷ ProjectBinding → ProjectBinding
simplify (ExpressionProj pr e) = ExpressionProj pr $ simplifyProj e
simplify p                     = p

-- |Helper function: concatMap for NonEmpty
neConcatMap ∷ (a → NE.NonEmpty b) → NE.NonEmpty a → NE.NonEmpty b
neConcatMap f = sconcat . NE.map f

-- |Simplify a project expression structure
--  1) transform singleton collections into it's only child
--  2) flatten same constructor of the collection
simplifyProj ∷ Project → Project
simplifyProj (SumProj (p NE.:| []))      = simplifyProj p
simplifyProj (ProductProj (p NE.:| []))  = simplifyProj p
simplifyProj (SequenceProj (p NE.:| [])) = simplifyProj p
simplifyProj (SumProj ps) =
    SumProj $ neConcatMap reduce ps
  where
    reduce (SumProj ps') = neConcatMap reduce ps'
    reduce p             = [simplifyProj p]
simplifyProj (ProductProj ps) =
    ProductProj $ neConcatMap reduce ps
  where
    reduce (ProductProj ps') = neConcatMap reduce ps'
    reduce p                 = [simplifyProj p]
simplifyProj (SequenceProj ps) =
    SequenceProj $ neConcatMap reduce ps
  where
    reduce (SequenceProj ps') = neConcatMap reduce ps'
    reduce p                  = [simplifyProj p]
simplifyProj p@RefProj {}     = p
