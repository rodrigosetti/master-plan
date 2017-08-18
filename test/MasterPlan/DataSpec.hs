{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module MasterPlan.DataSpec (spec) where

import           Control.Monad.State
import           Data.Bool             (bool)
import qualified Data.List.NonEmpty    as NE
import           MasterPlan.Arbitrary  ()
import           MasterPlan.Data
import           System.Random
import           System.Random.Shuffle (shuffle')
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       hiding (sample)

-- |Sample the simulation model of the execution of a project.
-- It's a stateful computation with the random generator, which computes
-- a 2-tuple with a Boolean: whether the execution was successful (A Bernoulli
-- sample from trust), and the total actual cost incurred.
simulate ∷ RandomGen g ⇒ Project a → State g (Bool, Cost)
simulate (Atomic _ (Cost c) (Trust t) (Progress p)) =
   do r <- state $ randomR (0, 1)
      let remainingProgress =  1 - p
          effectiveTrust = p + t * remainingProgress
          effectiveCost = c * remainingProgress
      pure (effectiveTrust > r, Cost effectiveCost)

simulate (Annotated _)     = pure (True, 0)
simulate (Sequence _ ps)   = simulateConjunction $ NE.toList ps
simulate (Product _ ps)    = simulateConjunction $ NE.toList ps
simulate (Sum _ ps)        =
  simulate' $ NE.toList ps
 where
   simulate' ∷ RandomGen g ⇒ [Project a] → State g (Bool, Cost)
   simulate' [] = pure (False, 0)
   simulate' (p:rest) = do (success, c) <- simulate p
                           if success then
                             pure (True, c)
                           else
                             do (success', c') <- simulate' rest
                                pure (success', c + c')

-- |Helper function that samples from a sequence of projects to be executed in
-- order, and which all must be successful for the end result to be succesful.
-- This is the case for sequences, and products (in a particular permutation).
simulateConjunction ∷ RandomGen g ⇒ [Project a] → State g (Bool, Cost)
simulateConjunction []       = pure (True, 0)
simulateConjunction (p:rest) = do (success, c) <- simulate p
                                  if success then do
                                    (success', c') <- simulateConjunction rest
                                    pure (success', c + c')
                                  else
                                    pure (False, c)

-- |Compute a project's trust and cost via a Monte Carlo method of computing
-- the average of a handful of samples.
monteCarloTrustAndCost ∷ RandomGen g ⇒ Int → ProjectExpr → State g (Trust, Cost)
monteCarloTrustAndCost n p = do results <- replicateM n $ simulate p
                                let trusts = map (bool 0 1 . fst) results
                                    costs  = map snd results
                                pure (sum trusts / fromIntegral n,
                                      sum costs / fromIntegral n)

aproximatelyEqual ∷ (Show a, Real a, Fractional a) => a -> a -> a → a -> Property
aproximatelyEqual alpha beta x y =
   counterexample (show x ++ " /= " ++ show y) $ diff <= max relError beta
  where
    relError = alpha * max (abs x) (abs y)
    diff = abs $ x - y

spec ∷ Spec
spec = do
  describe "trust and cost" $ do

    let g = mkStdGen 837183

    let eq :: (Show a, Real a, Fractional a) => a -> a -> Property
        eq = aproximatelyEqual 0.05 0.05

    prop "monte-carlo and analytical implementations should agree" $ do
        let monteCarloAndAnalyticalMustAgree ∷ ProjectExpr -> Property
            monteCarloAndAnalyticalMustAgree p =
                counterexample "disagree on cost"  (cost'  `eq` cost  p) .&&.
                counterexample "disagree on trust" (trust' `eq` trust p)
              where
                (trust', cost') = evalState (monteCarloTrustAndCost 50000 p) g
        monteCarloAndAnalyticalMustAgree

  describe "simplification" $ do

    let eq :: (Show a, Real a, Fractional a) => a -> a -> Property
        eq = aproximatelyEqual 0.005 0.005

    prop "is irreductible" $ do
        let simplificationIsIrreductible :: ProjectExpr -> Property
            simplificationIsIrreductible p =
                let p' = simplify p
                    p'' = simplify p'
                 in p /= p' ==> p' == p''
        simplificationIsIrreductible

    prop "is stable" $ do
      let simplifyIsStable :: ProjectExpr -> Property
          simplifyIsStable p =
            let p' = simplify p
             in cost p `eq` cost p' .&&. trust p `eq` trust p'

      simplifyIsStable

  describe "prioritization" $ do

    let shuffleProjs :: NE.NonEmpty (Project a) -> IO (NE.NonEmpty (Project a))
        shuffleProjs ps = do ps' <- NE.toList <$> mapM shuffleProj ps
                             g <- newStdGen
                             pure $ NE.fromList $ shuffle' ps' (length ps') g

        shuffleProj :: Project a -> IO (Project a)
        shuffleProj (Sum r ps)     = Sum r <$> shuffleProjs ps
        shuffleProj (Product r ps) = Product r <$> shuffleProjs ps
        shuffleProj p              = pure p

    prop "minimize cost and keep trust stable" $ do
      -- This test verifies that for any arbitrary project tree, the
      -- prioritized version of it will have the minimum cost.

      let eq = aproximatelyEqual 0.005 0.005

      let prioritizeMinimizesCost :: ProjectExpr -> Property
          prioritizeMinimizesCost p =
            let op = prioritize p
                ocost = cost op
                otrust = trust op
                costIsLessOrEqual p' =
                  counterexample ("variation has smaller cost: " ++ show p') $ ocost <= cost p'
                trustIsSame p' = otrust `eq` trust p'
            in ioProperty $ do variations <- replicateM 10 (shuffleProj p)
                               return $ conjoin (map costIsLessOrEqual variations) .&.
                                        conjoin (map trustIsSame variations)
      prioritizeMinimizesCost
