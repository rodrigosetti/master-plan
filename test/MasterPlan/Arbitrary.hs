{-# LANGUAGE UnicodeSyntax #-}
module MasterPlan.Arbitrary where

import           Control.Monad             (replicateM)
import qualified Data.List.NonEmpty        as NE
import qualified Data.Map                  as M
import           Data.Maybe                (isNothing)
import           MasterPlan.Data
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance Arbitrary ProjectProperties where

  arbitrary =
      let s = getASCIIString <$> arbitrary
          os = oneof [pure Nothing, Just <$> s]
      in ProjectProperties <$> s <*> os <*> os <*> os

  shrink p = (if isNothing (description p) then [] else [p { description = Nothing }]) ++
             (if isNothing (url p) then [] else [p { url = Nothing }]) ++
             (if isNothing (owner p) then [] else [p { owner = Nothing }])

testingKeys ∷ [String]
testingKeys = ["a", "b", "c", "d"]

instance Arbitrary ProjectSystem where

  arbitrary = do bs <- replicateM (length testingKeys) arbitrary
                 rootB <- ExpressionProj <$> arbitrary <*> arbitrary
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
    let unitGen = elements [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
     in frequency [ (50, TaskProj <$> arbitrary
                                  <*> elements [0, 1 .. 100]
                                  <*> unitGen
                                  <*> unitGen)
                  , (1, pure $ UnconsolidatedProj defaultProjectProps) ]

  shrink (ExpressionProj pr e) = map (ExpressionProj pr) $ shrink e
  shrink _                     = []

instance Arbitrary Project where

  arbitrary =
    let shrinkFactor n = 3 * n `quot` 5
    in  frequency [ (1, SumProj <$> scale shrinkFactor arbitrary)
                  , (1, ProductProj <$> scale shrinkFactor arbitrary)
                  , (1, SequenceProj <$> scale shrinkFactor arbitrary)
                  , (2, RefProj <$> elements testingKeys) ]

  shrink (SumProj ps)      = NE.toList ps
  shrink (ProductProj ps)  = NE.toList ps
  shrink (SequenceProj ps) = NE.toList ps
  shrink (RefProj _)       = []
