{-# LANGUAGE UnicodeSyntax #-}
module MasterPlan.Arbitrary where

import           Control.Monad             (replicateM)
import qualified Data.List.NonEmpty        as NE
import qualified Data.Map                  as M
import           MasterPlan.Data
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance Arbitrary ProjectProperties where

  --arbitrary = pure defaultProjectProps
  --{-
  arbitrary =
      let s = getPrintableString <$> arbitrary
          os = oneof [pure Nothing, Just <$> s]
      in ProjectProperties <$> s <*> os <*> os <*> os
{-
  shrink p = [ p { title = t } | t <- shrink $ title p ] ++
             [ p { description = t } | t <- shrink $ description p ] ++
             [ p { url = t } | t <- shrink $ url p ] ++
             [ p { owner = t } | t <- shrink $ owner p ]
  --}

instance Arbitrary Status where

  arbitrary = elements [ Ready, Blocked, Progress, Done, Cancelled ]

  shrink Done = []
  shrink _    = [Done]

testingKeys ∷ [String]
--testingKeys = ["a","b","c","d"]
testingKeys = ["a"]

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
    let unitGen = elements [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
     in frequency [ (50, TaskProj <$> arbitrary
                                  <*> elements [0, 1 .. 100]
                                  <*> unitGen
                                  <*> arbitrary
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

  shrink p = let p' = simplifyProj p in if p == p' then [] else [p]
  -- shrink (SumProj (p NE.:| []))      = [p]
  -- shrink (ProductProj (p NE.:| []))  = [p]
  -- shrink (SequenceProj (p NE.:| [])) = [p]
  -- shrink (SumProj ps)      = map SumProj (shrink ps) ++ NE.toList ps
  -- shrink (ProductProj ps)  = map ProductProj (shrink ps) ++ NE.toList ps
  -- shrink (SequenceProj ps) = map SequenceProj (shrink ps) ++ NE.toList ps
  -- shrink (RefProj _)       = []
