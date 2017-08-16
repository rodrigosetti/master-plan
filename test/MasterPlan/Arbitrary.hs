{-# LANGUAGE UnicodeSyntax #-}
module MasterPlan.Arbitrary () where

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
                 rootB <- BindingExpr <$> arbitrary <*> arbitrary
                 pure $ ProjectSystem $ M.insert "root" rootB $ M.fromList $ zip testingKeys bs

  shrink (ProjectSystem bs) =
      map ProjectSystem $ concatMap shrinkOne testingKeys
    where
      shrinkOne ∷ String → [M.Map String Binding]
      shrinkOne k = case M.lookup k bs of
        Nothing -> []
        Just b  -> map (\s -> M.adjust (const s) k bs) $ shrink b

instance Arbitrary Binding where

  -- NOTE: Binding arbitrary are always tasks (no expression)
  --       to avoid generating cycles
  arbitrary =
    let unitGen = elements [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
     in BindingAtomic <$> arbitrary
                      <*> elements [0, 1 .. 100]
                      <*> unitGen
                      <*> unitGen

  shrink (BindingExpr pr e) = map (BindingExpr pr) $ shrink e
  shrink _                     = []

instance Arbitrary ProjectExpr where

  arbitrary =
    let shrinkFactor n = 3 * n `quot` 5
    in  frequency [ (1, Sum <$> scale shrinkFactor arbitrary)
                  , (1, Product <$> scale shrinkFactor arbitrary)
                  , (1, Sequence <$> scale shrinkFactor arbitrary)
                  , (2, Reference <$> elements testingKeys) ]

  shrink (Sum ps)      = NE.toList ps
  shrink (Product ps)  = NE.toList ps
  shrink (Sequence ps) = NE.toList ps
  shrink (Reference _)       = []
