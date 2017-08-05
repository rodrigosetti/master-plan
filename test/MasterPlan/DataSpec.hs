module MasterPlan.DataSpec where

import           Data.Bool           (bool)
import qualified Data.Map            as M
import           MasterPlan.Data
import           Test.Hspec
import           Test.QuickCheck     hiding (sample)

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.State
import qualified Data.List.NonEmpty  as NE
import           System.Random

instance Arbitrary ProjectProperties where

  arbitrary = pure defaultProjectProps
{-
  arbitrary = ProjectProperties <$> arbitrary
                                <*> arbitrary
                                <*> arbitrary
                                <*> arbitrary

  shrink p = [ p { name = t } | t <- shrink $ name p ] ++
             [ p { description = t } | t <- shrink $ description p ] ++
             [ p { url = t } | t <- shrink $ url p ] ++
             [ p { owner = t } | t <- shrink $ owner p ]
-}

instance Arbitrary Status where

  arbitrary = oneof [ pure Ready, pure Blocked, pure InProgress, pure Done, pure Cancelled ]

testingKeys :: [String]
testingKeys = ["a","b","c","d"]

rootKey :: String
rootKey = "root"

instance Arbitrary ProjectSystem where

  arbitrary = do bs <- replicateM (length testingKeys) arbitrary
                 let arbitraryExpr = ExpressionProj <$> arbitrary <*> arbitrary
                 rootB <- frequency [ (1, arbitrary), (10, arbitraryExpr) ]
                 let bindings = M.insert rootKey rootB $ M.fromList $ zip testingKeys bs
                 pure $ ProjectSystem bindings

  shrink (ProjectSystem bs) =
      map ProjectSystem $ concatMap shrinkOne testingKeys
    where
      shrinkOne :: String -> [M.Map String ProjectBinding]
      shrinkOne k = case M.lookup k bs of
        Nothing -> []
        Just b  -> map (\s -> M.adjust (const s) k bs) $ shrink b

instance Arbitrary ProjectBinding where

  -- NOTE: ProjectBinding arbitrary are always tasks (no expression)
  --       to avoid generating cycles
  arbitrary =
    let unitGen = choose (0.0, 1.0)
     in TaskProj <$> arbitrary
                 <*> unitGen
                 <*> unitGen
                 <*> arbitrary
                 <*> unitGen

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

average :: RandomGen g => State g Float -> Int -> State g Float
average sample n = do total <- replicateM n sample
                      pure $ sum total / fromIntegral n

simulate :: RandomGen g => ProjectSystem -> Project -> State g (Bool, Cost)
simulate sys (RefProj n) =
   case M.lookup n (bindings sys) of
     Just TaskProj { reportedTrust=t, reportedCost=c }    ->
       do r <- state $ randomR (0, 1)
          pure (t > r, c)
     Just ExpressionProj { expression=p} -> simulate sys p -- TODO: avoid cyclic
     Nothing                             -> pure (False, 0) -- should not happen

simulate sys SequenceProj { subprojects=ps }   = simulateConjunction sys $ NE.dropWhile (isClosed sys) ps
simulate sys ProductProj { subprojects=ps }    = simulateConjunction sys $ NE.filter (isOpen sys) ps
simulate sys SumProj { subprojects=ps }        =
  if null opens then pure (True, 0) else simulate' opens
 where
   opens = NE.filter (isOpen sys) ps
   simulate' :: RandomGen g => [Project] -> State g (Bool, Cost)
   simulate' [] = pure (False, 0)
   simulate' (p:rest) = do (success, c) <- simulate sys p
                           if success then
                             pure (True, c)
                           else
                             do (success', c') <- simulate' rest
                                pure (success', c + c')

simulateConjunction :: RandomGen g => ProjectSystem -> [Project] -> State g (Bool, Cost)
simulateConjunction _ []       = pure (True, 0)
simulateConjunction sys (p:rest) = do (success, c) <- simulate sys p
                                      if success then do
                                        (success', c') <- simulateConjunction sys rest
                                        pure (success', c + c')
                                      else
                                        pure (False, c)

monteCarloTrusteAndCost :: RandomGen g => Int -> ProjectSystem -> Project -> State g (Trust, Cost)
monteCarloTrusteAndCost n sys p = do results <- replicateM n $ simulate sys p
                                     let trusts = map (bool 0 1 . fst) results
                                     let costs = map snd results
                                     pure (sum trusts / fromIntegral n,
                                           sum costs / fromIntegral n)

aproximatelyEqual :: Float -> Float -> Property
aproximatelyEqual x y =
   counterexample (show x ++ " /= " ++ show y) (abs (x - y) <= epislon)
  where
    epislon = 0.05

spec :: Spec
spec = do
  describe "estimations" $ do

    let g = mkStdGen 837183

    it "monte-carlo and analytical implementations should agree on cost" $ do
      let propertyMCAndAnalyticalEq :: ProjectSystem -> Property
          propertyMCAndAnalyticalEq sys =
            cost' `aproximatelyEqual` cost sys p
           where
             p = RefProj rootKey
             (_, cost') = evalState (monteCarloTrusteAndCost 10000 sys p) g

      property propertyMCAndAnalyticalEq

    it "monte-carlo and analytical implementations should agree on trust" $ do
        let propertyMCAndAnalyticalEq :: ProjectSystem -> Property
            propertyMCAndAnalyticalEq sys =
              trust' `aproximatelyEqual` trust sys p
             where
               p = RefProj rootKey
               (trust', _) = evalState (monteCarloTrusteAndCost 10000 sys p) g

        property propertyMCAndAnalyticalEq

  describe "cost" $ do
    let p1 = TaskProj { props=defaultProjectProps
                     , reportedCost = 10
                     , reportedTrust = 0.8
                     , reportedProgress=1
                     , reportedStatus = Done }
    let p2 = TaskProj { props=defaultProjectProps
                     , reportedCost = 5
                     , reportedTrust = 1
                     , reportedProgress = 0.2
                     , reportedStatus = InProgress }
    let p3 = TaskProj { props=defaultProjectProps
                      , reportedCost = 7
                      , reportedTrust = 1
                      , reportedProgress = 0
                      , reportedStatus = Ready }
    let p4 = TaskProj { props=defaultProjectProps
                     , reportedCost = 2
                     , reportedTrust = 1
                     , reportedProgress = 0
                     , reportedStatus = Ready }

    it "is correct for sequences" $ do
      let p = SequenceProj $ NE.fromList $ map RefProj ["p1", "p2", "p3", "p4"]
          sys = ProjectSystem $ M.fromList $ zip ["p1", "p2", "p3", "p4"] [p1, p2, p3, p4]
      cost sys p `shouldBe` 14

    it "is correct for products" $ do
      let p = ProductProj $ NE.fromList $ map RefProj ["p1", "p2", "p3", "p4"]
          sys = ProjectSystem $ M.fromList $ zip ["p1", "p2", "p3", "p4"] [p1, p2, p3, p4]
      cost sys p `shouldBe` 14
