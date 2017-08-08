{-# LANGUAGE UnicodeSyntax #-}
module MasterPlan.DataSpec (spec) where

import           Control.Monad.State
import           Data.Bool             (bool)
import qualified Data.List.NonEmpty    as NE
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)
import           MasterPlan.Arbitrary  ()
import           MasterPlan.Data
import           System.Random
import           System.Random.Shuffle (shuffle')
import           Test.Hspec
import           Test.QuickCheck       hiding (sample)

average ∷ RandomGen g ⇒ State g Float → Int → State g Float
average sample n = do tot <- replicateM n sample
                      pure $ sum tot / fromIntegral n

simulate ∷ RandomGen g ⇒ ProjectSystem → Project → State g (Bool, Cost)
simulate sys (RefProj n) =
   case M.lookup n (bindings sys) of
     Just (TaskProj _ c t p) ->
       do r <- state $ randomR (0, 1)
          let remainingProgress =  1 - p
              effectiveTrust = p + t * remainingProgress
              effectiveCost = c * remainingProgress
          pure (effectiveTrust > r, effectiveCost)
     Just (ExpressionProj _ p)           -> simulate sys p -- TODO: avoid cyclic
     Just (UnconsolidatedProj _)         -> pure (True, 0)
     Nothing                             -> pure (False, 0) -- should not happen

simulate sys (SequenceProj ps)   = simulateConjunction sys $ NE.toList ps
simulate sys (ProductProj ps)    = simulateConjunction sys $ NE.toList ps
simulate sys (SumProj ps)        =
  simulate' $ NE.toList ps
 where
   simulate' ∷ RandomGen g ⇒ [Project] → State g (Bool, Cost)
   simulate' [] = pure (False, 0)
   simulate' (p:rest) = do (success, c) <- simulate sys p
                           if success then
                             pure (True, c)
                           else
                             do (success', c') <- simulate' rest
                                pure (success', c + c')

simulateConjunction ∷ RandomGen g ⇒ ProjectSystem → [Project] → State g (Bool, Cost)
simulateConjunction _ []       = pure (True, 0)
simulateConjunction sys (p:rest) = do (success, c) <- simulate sys p
                                      if success then do
                                        (success', c') <- simulateConjunction sys rest
                                        pure (success', c + c')
                                      else
                                        pure (False, c)

monteCarloTrustAndCost ∷ RandomGen g ⇒ Int → ProjectSystem → Project → State g (Trust, Cost)
monteCarloTrustAndCost n sys p = do results <- replicateM n $ simulate sys p
                                    let trusts = map (bool 0 1 . fst) results
                                    let costs = map snd results
                                    pure (sum trusts / fromIntegral n,
                                         sum costs / fromIntegral n)

aproximatelyEqual ∷ Float -> Float -> Float → Float -> Property
aproximatelyEqual alpha beta x y =
   counterexample (show x ++ " /= " ++ show y) $ diff <= max relError beta
  where
    relError = alpha * max (abs x) (abs y)
    diff = abs $ x - y

spec ∷ Spec
spec = do
  describe "trust and cost" $ do

    let g = mkStdGen 837183

    let eq = aproximatelyEqual 0.05 0.05

    it "monte-carlo and analytical implementations should agree" $ do
        let monteCarloAndAnalyticalAgree ∷ ProjectSystem → Property
            monteCarloAndAnalyticalAgree sys =
              (counterexample "disagree on cost"  $ cost'  `eq` cost  sys p) .&&.
              (counterexample "disagree on trust" $ trust' `eq` trust sys p)
             where
               p = RefProj rootKey
               (trust', cost') = evalState (monteCarloTrustAndCost 50000 sys p) g

        property monteCarloAndAnalyticalAgree

  describe "simplification" $ do

    let eq = aproximatelyEqual 0.005 0.005

    it "is irreductible" $ do
      let simplificationIsIrreductible :: Project -> Property
          simplificationIsIrreductible p =
            let p' = simplifyProj p
                p'' = simplifyProj p'
             in p /= p' ==> p' == p''

      property simplificationIsIrreductible

    it "is stable" $ do
      let propSimplifyIsStable :: ProjectSystem -> Property
          propSimplifyIsStable sys =
            let sys' = sys { bindings = M.map simplify $ bindings sys }
                p    = RefProj rootKey
             in cost sys p `eq` cost sys' p .&&. trust sys p `eq` trust sys' p

      property propSimplifyIsStable

  describe "optimization" $ do

    let shuffleProjs :: NE.NonEmpty Project -> IO (NE.NonEmpty Project)
        shuffleProjs ps = do ps' <- NE.toList <$> mapM shuffleProj ps
                             g <- newStdGen
                             pure $ NE.fromList $ shuffle' ps' (length ps') g

        shuffleProj :: Project -> IO Project
        shuffleProj (SumProj ps)      = SumProj <$> shuffleProjs ps
        shuffleProj (ProductProj ps)  = ProductProj <$> shuffleProjs ps
        shuffleProj p                 = pure p

    it "minimize cost and keep trust stable" $ do
      -- This test verifies that for any arbitrary project tree, the
      -- optimized version of it will have the minimum cost.

      let eq = aproximatelyEqual 0.005 0.005

      let optimizeMinimizesCost :: ProjectSystem -> Property
          optimizeMinimizesCost sys =
            let (ExpressionProj _ p) = fromJust $ M.lookup "root" $ bindings sys
                op = optimizeProj sys p
                ocost = cost sys op
                otrust = trust sys op
                costIsLessOrEqual p' =
                  counterexample "cost is >" $ ocost <= c .||. ocost `eq` c where c = cost sys p'
                trustIsSame p' = otrust `eq` t where t = trust sys p'
            in ioProperty $ do variations <- replicateM 10 (shuffleProj p)
                               return $ conjoin (map costIsLessOrEqual variations) .&.
                                        conjoin (map trustIsSame variations)
      property optimizeMinimizesCost
