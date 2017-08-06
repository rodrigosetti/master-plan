{-# LANGUAGE UnicodeSyntax #-}
module MasterPlan.Arbitrary where

import           Control.Monad      (replicateM)
import           Data.List          (nub)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import           MasterPlan.Data
import           Test.QuickCheck

instance Arbitrary ProjectProperties where

  arbitrary = pure defaultProjectProps
  {-
  arbitrary = ProjectProperties <$> arbitrary
                                <*> arbitrary
                                <*> arbitrary
                                <*> arbitrary

  shrink p = [ p { title = t } | t <- shrink $ title p ] ++
             [ p { description = t } | t <- shrink $ description p ] ++
             [ p { url = t } | t <- shrink $ url p ] ++
             [ p { owner = t } | t <- shrink $ owner p ]
             -}

instance Arbitrary Status where

  arbitrary = elements [ Ready, Blocked, InProgress, Done, Cancelled ]

  shrink Done = []
  shrink _    = [Done]

testingKeys ∷ [String]
testingKeys = ["a","b","c","d"]

rootKey ∷ String
rootKey = "root"

instance Arbitrary ProjectSystem where

  arbitrary = do bs <- replicateM (length testingKeys) arbitrary
                 let arbitraryExpr = ExpressionProj <$> arbitrary <*> arbitrary
                 rootB <- frequency [ (1, arbitrary), (10, arbitraryExpr) ]
                 pure $ ProjectSystem $ M.insert rootKey rootB $ M.fromList $ zip testingKeys bs

  shrink (ProjectSystem bs) =
      map ProjectSystem $ concatMap shrinkOne testingKeys
    where
      shrinkOne ∷ String → [M.Map String ProjectBinding]
      shrinkOne k = case M.lookup k bs of
        Nothing -> []
        Just b  -> map (\s -> M.adjust (const s) k bs) $ shrink b

instance Arbitrary ProjectBinding where

  -- NOTE: ProjectBinding arbitrary are always tasks (no expression)
  --       to avoid generating cycles
  arbitrary =
    let unitGen = choose (0.0, 1.0)
     in frequency [ (50, TaskProj <$> arbitrary
                                  <*> unitGen
                                  <*> unitGen
                                  <*> arbitrary
                                  <*> unitGen)
                  , (1, pure $ UnconsolidatedProj defaultProjectProps) ]

  shrink b = nub [ b { reportedCost=0 }
                 , b { reportedCost=1 }
                 , b { reportedTrust=0 }
                 , b { reportedTrust=1 }
                 , b { reportedStatus=Done } ]

instance Arbitrary Project where

  arbitrary =
    let shrinkFactor n = 2 * n `quot` 5
    in  oneof [ SumProj <$> scale shrinkFactor arbitrary
              , ProductProj <$> scale shrinkFactor arbitrary
              , SequenceProj <$> scale shrinkFactor arbitrary
              , RefProj <$> elements testingKeys ]

  shrink (SumProj ps)      = map SumProj (shrink ps) ++ NE.toList ps
  shrink (ProductProj ps)  = map ProductProj (shrink ps) ++ NE.toList ps
  shrink (SequenceProj ps) = map SequenceProj (shrink ps) ++ NE.toList ps
  shrink (RefProj _)       = []
