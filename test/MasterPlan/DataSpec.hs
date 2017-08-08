{-# LANGUAGE UnicodeSyntax #-}
module MasterPlan.DataSpec where

import           Data.Bool            (bool)
import qualified Data.Map             as M
import           MasterPlan.Data
import           Test.Hspec
import           Test.QuickCheck      hiding (sample)

import           Control.Monad.State
import qualified Data.List.NonEmpty   as NE
import           MasterPlan.Arbitrary ()
import           System.Random

average ∷ RandomGen g ⇒ State g Float → Int → State g Float
average sample n = do tot <- replicateM n sample
                      pure $ sum tot / fromIntegral n

simulate ∷ RandomGen g ⇒ ProjectSystem → Project → State g (Bool, Cost)
simulate sys (RefProj n) =
   case M.lookup n (bindings sys) of
     Just TaskProj { reportedTrust=t, reportedCost=c, reportedProgress=p } ->
       do r <- state $ randomR (0, 1)
          let remainingProgress =  1 - p
              effectiveTrust = p + t * remainingProgress
              effectiveCost = c * remainingProgress
          pure (effectiveTrust > r, effectiveCost)
     Just ExpressionProj { expression=p} -> simulate sys p -- TODO: avoid cyclic
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
  describe "estimations" $ do

    let g = mkStdGen 837183

    let eq = aproximatelyEqual 0.05 0.05

    it "monte-carlo and analytical implementations should agree on cost" $ do
        let propertyMCAndAnalyticalEq ∷ ProjectSystem → Property
            propertyMCAndAnalyticalEq sys =
              cost' `eq` cost sys p
             where
               p = RefProj rootKey
               (_, cost') = evalState (monteCarloTrustAndCost 10000 sys p) g

        property propertyMCAndAnalyticalEq

    it "monte-carlo and analytical implementations should agree on trust" $ do
        let propertyMCAndAnalyticalEq ∷ ProjectSystem → Property
            propertyMCAndAnalyticalEq sys =
              trust' `eq` trust sys p
             where
               p = RefProj rootKey
               (trust', _) = evalState (monteCarloTrustAndCost 10000 sys p) g

        property propertyMCAndAnalyticalEq

  describe "simplify" $ do

    let eq = aproximatelyEqual 0.005 0.005

    it "is irreductible" $ do
      let simplificationIsIrreductible :: Project -> Property
          simplificationIsIrreductible p =
            let p' = simplifyProj p
                p'' = simplifyProj p'
             in p /= p' ==> p' == p''

      property simplificationIsIrreductible

    it "should not change the estimations" $ do
      let propSimplifyIsStable :: ProjectSystem -> Property
          propSimplifyIsStable sys =
            let sys' = sys { bindings = M.map simplify $ bindings sys }
                p    = RefProj rootKey
             in cost sys p `eq` cost sys' p .&&. trust sys p `eq` trust sys' p

      property propSimplifyIsStable
