module MasterPlan.Data where

import           Data.Foldable      (asum)
import           Data.List          (find, inits)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe         (fromMaybe)

type Percentage = Float
type Cost = Float

-- | properties that common to composed and atomic projects
data ProjectProperties = ProjectProperties {
  name        :: String,
  description :: Maybe[String],
  url         :: Maybe[String],
  owner       :: Maybe[String]
} deriving (Eq, Show)

data Status = Ready | Blocked | InProgress | Done | Cancelled
          deriving (Eq, Show)

data Project = SumProj {
                  props       :: ProjectProperties,
                  subprojects :: NE.NonEmpty Project
               } |
               ProductProj {
                  props       :: ProjectProperties,
                  subprojects :: NE.NonEmpty Project
               } |
               SequenceProj {
                  props       :: ProjectProperties,
                  subprojects :: NE.NonEmpty Project
               } |
               TaskProj {
                  props              :: ProjectProperties,
                  reportedCost       :: Cost,
                  reportedConfidence :: Percentage,
                  reportedStatus     :: Status,
                  reportedProgress   :: Percentage
               }
                deriving (Eq, Show)


defaultProjectProps :: ProjectProperties
defaultProjectProps = ProjectProperties { name = "root"
                                        , description = Nothing
                                        , url = Nothing
                                        , owner = Nothing }

defaultTaskProj :: Project
defaultTaskProj = TaskProj { props = defaultProjectProps
                           , reportedCost = 0
                           , reportedConfidence = 1
                           , reportedStatus = Ready
                           , reportedProgress = 0 }

isOpen :: Project -> Bool
isOpen p = status p `elem` [InProgress, Ready, Blocked]

isClosed :: Project -> Bool
isClosed = not . isOpen

-- | Expected cost
cost :: Project -> Cost
cost TaskProj { reportedCost=c } = c
cost SequenceProj { subprojects=ps } = costConjunction $ NE.dropWhile isClosed ps
cost ProductProj { subprojects=ps } = costConjunction $ NE.filter isOpen ps
cost SumProj { subprojects=s } =
   final_cost
 where
   final_prob = scanl (\a b -> a + b*(1-a)) 0 $ map confidence opens
   final_cost = sum $ map (\x -> (1 - snd x) * fst x) $ zip costs final_prob
   costs = map cost opens
   opens = NE.filter isOpen s

costConjunction :: [Project] -> Cost
costConjunction ps =
   sum $ zipWith (*) costs accConfidences
  where
    costs = map cost ps
    accConfidences = map product $ inits $ map confidence ps

-- | Expected confidence probability
confidence :: Project -> Percentage
confidence TaskProj { reportedConfidence=c } = c
confidence SequenceProj { subprojects=ps } = confidenceConjunction $ NE.dropWhile isClosed ps
confidence ProductProj { subprojects=ps } = confidenceConjunction $ NE.filter isOpen ps
confidence SumProj { subprojects=s } =
  if null opens then 1 else final_prob
 where
  final_prob = foldl (\a b -> a + b*(1-a)) 0 $ map confidence opens
  opens = NE.filter isOpen s

confidenceConjunction :: [Project] -> Percentage
confidenceConjunction ps = product $ map confidence ps

progress :: Project -> Percentage
progress TaskProj { reportedProgress=p, reportedStatus=s } = if s == Done then 1 else p
progress SequenceProj { subprojects=s } = progressConjunction s
progress ProductProj { subprojects=s } = progressConjunction s
progress SumProj { subprojects=s } = maximum $ NE.map progress s

progressConjunction :: NE.NonEmpty Project -> Percentage
progressConjunction ps =
   let opens = NE.filter isOpen ps
    in if null opens
      then 1
      else sum (map progress opens) / fromIntegral (length opens)

status :: Project -> Status
status TaskProj { reportedProgress=p, reportedStatus=s } = if p >= 1 then Done else s
status SequenceProj { subprojects=s } =
  let rest = NE.dropWhile isClosed s
  in case rest of (p : _) -> status p
                  []      -> Done
status ProductProj { subprojects=ps } =
  statusPriority [InProgress, Ready, Blocked, Cancelled, Done] ps
status SumProj { subprojects=ps } =
  statusPriority [Done, InProgress, Ready, Blocked, Cancelled] ps

statusPriority :: [Status] -> NE.NonEmpty Project -> Status
statusPriority priority ps =
  let ss = NE.map status ps
   in fromMaybe Done $ asum $ map (\x -> find (x ==) ss) priority
