{-# LANGUAGE UnicodeSyntax #-}
module MasterPlan.Data where

import           Data.Foldable      (asum)
import           Data.List          (find, inits)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import           Data.Maybe         (fromMaybe)

type Trust = Float
type Cost = Float
type Progress = Float

data Status = Ready | Blocked | InProgress | Done | Cancelled
          deriving (Eq, Show)

-- |Structure of a project expression
data Project = SumProj { subprojects :: NE.NonEmpty Project } |
               ProductProj { subprojects :: NE.NonEmpty Project } |
               SequenceProj { subprojects :: NE.NonEmpty Project } |
               RefProj { name :: String }
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
                                     }
                         deriving (Eq, Show)

-- |Any binding (with a name) may have associated properties
data ProjectProperties = ProjectProperties { title       :: String
                                           , description :: Maybe[String]
                                           , url         :: Maybe[String]
                                           , owner       :: Maybe[String]
                                           } deriving (Eq, Show)

-- |A project system defines the bindins (mapping from names to expressions or tasks)
-- and properties, which can be associated to any binding
newtype ProjectSystem = ProjectSystem { bindings :: M.Map String ProjectBinding }
                          deriving (Eq, Show)

defaultProjectProps ∷ ProjectProperties
defaultProjectProps = ProjectProperties { title = "root"
                                        , description = Nothing
                                        , url = Nothing
                                        , owner = Nothing }

isOpen ∷ ProjectSystem → Project → Bool
isOpen sys p = status sys p `elem` [InProgress, Ready, Blocked]

isClosed ∷ ProjectSystem → Project → Bool
isClosed sys p = not $ isOpen sys p

-- | Expected cost
cost ∷ ProjectSystem → Project → Cost
cost sys (RefProj n) =
  case M.lookup n (bindings sys) of
    Just TaskProj { reportedCost=c }    -> c
    Just ExpressionProj { expression=p} -> cost sys p -- TODO: avoid cyclic
    Nothing                             -> 0 -- should not happen
cost sys (SequenceProj ps) = costConjunction sys $ NE.dropWhile (isClosed sys) ps
cost sys (ProductProj ps) = costConjunction sys $ NE.filter (isOpen sys) ps
cost sys (SumProj ps) =
   sum $ map (\x -> (1 - snd x) * fst x) $ zip costs accTrusts
 where
   accTrusts = scanl (\a b -> a + b*(1-a)) 0 $ map (trust sys) opens
   costs = map (cost sys) opens
   opens = NE.filter (isOpen sys) ps

costConjunction ∷ ProjectSystem → [Project] → Cost
costConjunction sys ps =
   sum $ zipWith (*) costs accTrusts
  where
    costs = map (cost sys) ps
    accTrusts = map product $ inits $ map (trust sys) ps

-- | Expected trust probability
trust ∷ ProjectSystem → Project → Trust
trust sys (RefProj n) =
  case M.lookup n (bindings sys) of
    Just TaskProj { reportedTrust=t }   -> t
    Just ExpressionProj { expression=p} -> trust sys p -- TODO: avoid cyclic
    Nothing                             -> 0 -- should not happen
trust sys (SequenceProj ps) = trustConjunction sys $ NE.dropWhile (isClosed sys) ps
trust sys (ProductProj ps) = trustConjunction sys $ NE.filter (isOpen sys) ps
trust sys (SumProj ps) =
  if null opens then 1 else accTrusts
 where
  accTrusts = foldl (\a b -> a + b*(1-a)) 0 $ map (trust sys) opens
  opens = NE.filter (isOpen sys) ps

trustConjunction ∷ ProjectSystem → [Project] → Trust
trustConjunction sys ps = product $ map (trust sys) ps

progress ∷ProjectSystem → Project → Progress
progress sys (RefProj n) =
  case M.lookup n (bindings sys) of
    Just TaskProj { reportedStatus=Done } -> 1
    Just TaskProj { reportedProgress=p }  -> p
    Just ExpressionProj { expression=p}   -> progress sys p -- TODO: avoid cyclic
    Nothing                               -> 0 -- should not happen
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
    Nothing                                        -> Cancelled -- should not happen
status sys (SequenceProj ps) =
  let rest = NE.dropWhile (isClosed sys) ps
  in case rest of (p : _) -> status sys p
                  []      -> Done
status sys (ProductProj ps) =
  statusPriority [InProgress, Ready, Blocked, Cancelled, Done] sys ps
status sys (SumProj ps) =
  statusPriority [Done, InProgress, Ready, Blocked, Cancelled] sys ps

statusPriority ∷ [Status] → ProjectSystem → NE.NonEmpty Project → Status
statusPriority priority sys ps =
  let ss = NE.map (status sys) ps
   in fromMaybe Done $ asum $ map (\x -> find (x ==) ss) priority
