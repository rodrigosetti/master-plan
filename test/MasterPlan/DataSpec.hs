module MasterPlan.DataSpec where

import           Data.Bool           (bool)
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

instance Arbitrary Project where

  arbitrary =
    let shrinkFactor n = 2 * n `quot` 5
        unitGen = choose (0.0, 1.0)
    in  oneof [ SumProj <$> arbitrary <*> scale shrinkFactor arbitrary
              , ProductProj <$> arbitrary <*> scale shrinkFactor arbitrary
              , SequenceProj <$> arbitrary <*> scale shrinkFactor arbitrary
              , TaskProj <$> arbitrary
                         <*> unitGen
                         <*> unitGen
                         <*> arbitrary
                         <*> unitGen ]

  shrink (SumProj p ps)      = map (SumProj p) (shrink ps) ++ NE.toList ps
  shrink (ProductProj p ps)  = map (ProductProj p) (shrink ps) ++ NE.toList ps
  shrink (SequenceProj p ps) = map (SequenceProj p) (shrink ps) ++ NE.toList ps
  shrink TaskProj {}         = []

average :: RandomGen g => State g Float -> Int -> State g Float
average sample n = do total <- replicateM n sample
                      pure $ sum total / fromIntegral n

simulate :: RandomGen g => Project -> State g (Bool, Cost)
simulate TaskProj { reportedConfidence=t, reportedCost=c } =
  do n <- state $ randomR (0, 1)
     pure (t > n, c)

simulate SequenceProj { subprojects=ps }   = simulateConjunction $ NE.dropWhile isClosed ps
simulate ProductProj { subprojects=ps }    = simulateConjunction $ NE.filter isOpen ps
simulate SumProj { subprojects=ps }        =
  if null opens then pure (True, 0) else simulate' opens
 where
   opens = NE.filter isOpen ps
   simulate' :: RandomGen g => [Project] -> State g (Bool, Cost)
   simulate' [] = pure (False, 0)
   simulate' (p:rest) = do (success, c) <- simulate p
                           if success then
                             pure (True, c)
                           else
                             do (success', c') <- simulate' rest
                                pure (success', c + c')

simulateConjunction :: RandomGen g => [Project] -> State g (Bool, Cost)
simulateConjunction []       = pure (True, 0)
simulateConjunction (p:rest) = do (success, c) <- simulate p
                                  if success then do
                                     (success', c') <- simulateConjunction rest
                                     pure (success', c + c')
                                   else
                                     pure (False, c)

monteCarloConfidenceAndCost :: RandomGen g => Int -> Project -> State g (Percentage, Cost)
monteCarloConfidenceAndCost n p = do results <- replicateM n $ simulate p
                                     let confidences = map (bool 0 1 . fst) results
                                     let costs = map snd results
                                     pure (sum confidences / fromIntegral n,
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
      let propertyMCAndAnalyticalEq :: Project -> Property
          propertyMCAndAnalyticalEq p =
            cost' `aproximatelyEqual` cost p
           where
             (_, cost') = evalState (monteCarloConfidenceAndCost 10000 p) g

      property propertyMCAndAnalyticalEq

    it "monte-carlo and analytical implementations should agree on confidence" $ do
        let propertyMCAndAnalyticalEq :: Project -> Property
            propertyMCAndAnalyticalEq p =
              confidence' `aproximatelyEqual` confidence p
             where
               (confidence', _) = evalState (monteCarloConfidenceAndCost 10000 p) g

        property propertyMCAndAnalyticalEq

  describe "cost" $ do
    let p1 = defaultTaskProj { reportedCost = 10
                             , reportedConfidence = 0.8
                             , reportedProgress=1
                             , reportedStatus = Done }
    let p2 = defaultTaskProj { reportedCost = 5
                             , reportedConfidence = 1
                             , reportedProgress = 0.2
                             , reportedStatus = InProgress }
    let p3 = defaultTaskProj { reportedCost = 7
                             , reportedConfidence = 1
                             , reportedProgress = 0
                             , reportedStatus = Ready }
    let p4 = defaultTaskProj { reportedCost = 2
                             , reportedConfidence = 1
                             , reportedProgress = 0
                             , reportedStatus = Ready }

    it "is correct for sequences" $ do
      let p = SequenceProj defaultProjectProps $ NE.fromList [p1, p2, p3, p4]
      cost p `shouldBe` 14

    it "is correct for products" $ do
      let p = ProductProj defaultProjectProps $ NE.fromList [p1, p2, p3, p4]
      cost p `shouldBe` 14
