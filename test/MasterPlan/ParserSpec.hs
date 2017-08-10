{-# LANGUAGE UnicodeSyntax #-}
module MasterPlan.ParserSpec (spec) where

import           Data.Either                 (isRight)
import           MasterPlan.Arbitrary        ()
import           MasterPlan.Backend.Identity (render)
import           MasterPlan.Data
import           MasterPlan.Parser           (runParser)
import           Test.Hspec
import           Test.QuickCheck

spec ∷ Spec
spec =
  describe "parser" $ do

    let allProps = [minBound :: ProjProperty ..]

    it "rendered should be parseable" $ do
      let renderedIsParseable ∷ ProjectSystem → Property
          renderedIsParseable sys =
            let rendered = render sys allProps
             in counterexample rendered $ isRight (runParser "test1" rendered)

      property $ withMaxSuccess 50 renderedIsParseable

    it "identity backend output should parse into the same input" $ do

      let propertyParseAndOutputIdentity ∷ ProjectSystem → Property
          propertyParseAndOutputIdentity sys =
            let sys' = simplify sys
                parsed = runParser "test2" (render sys' allProps)
             in isRight parsed ==> parsed === Right sys'

      property $ withMaxSuccess 50 propertyParseAndOutputIdentity

    it "should reject recursive equations" $ do

      let expectedError _   (Right _) = False
          expectedError key (Left s) =
            let l = last $ lines s
             in l == "definition of \"" ++ key ++ "\" is recursive"

      let wrap = unlines . map (++ ";\n")

      -- obvious
      let program1 = wrap ["root = a + b + root"]

      runParser "recursive1" program1 `shouldSatisfy` expectedError "root"

      let program2 = wrap [ "root = a + b"
                          , "a = x * root" ]

      runParser "recursive2" program2 `shouldSatisfy` expectedError "a"

      let program3 = wrap [ "root = x + y"
                          , "a = b * c"
                          , "c = d -> a" ]

      runParser "recursive3" program3 `shouldSatisfy` expectedError "c"

      let program4 = wrap [ "root = a + y"
                          , "d = x + root"
                          , "a = b * c"
                          , "c = d -> e" ]

      runParser "recursive4" program4 `shouldSatisfy` expectedError "c"
