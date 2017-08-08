{-# LANGUAGE UnicodeSyntax #-}
module MasterPlan.ParserSpec (spec) where

import           Data.Either                 (isRight)
import qualified Data.Map                    as M
import           MasterPlan.Arbitrary        ()
import           MasterPlan.Backend.Identity (render)
import           MasterPlan.Data
import           MasterPlan.Parser           (runParser)
import           Test.Hspec
import           Test.QuickCheck

spec ∷ Spec
spec =
  describe "parser" $ do

    it "rendered should be parseable" $ do
      let renderedIsParseable ∷ ProjectSystem → Property
          renderedIsParseable sys =
            let rendered = render sys
             in counterexample rendered $ isRight (runParser "test1" rendered)

      property $ withMaxSuccess 50 renderedIsParseable

    it "identity backend output should parse into the same input" $ do

      let propertyParseAndOutputIdentity ∷ ProjectSystem → Property
          propertyParseAndOutputIdentity sys =
            let sys' = sys { bindings = M.map simplify $ bindings sys}
                parsed = runParser "test2" (render sys')
             in isRight parsed ==> parsed === Right sys'

      property $ withMaxSuccess 50 propertyParseAndOutputIdentity

    it "should reject recursive equations" $ do

      let expectedError key  = Left $ "definition of \"" ++ key ++ "\" is recursive\n"

      let wrap = unlines . map (++ ";\n")

      -- obvious
      let program1 = wrap ["root = a + b + root"]

      runParser "recursive1" program1 `shouldBe` expectedError "root"

      let program2 = wrap [ "root = a + b"
                          , "a = x * root" ]

      runParser "recursive2" program2 `shouldBe` expectedError "a"

      let program3 = wrap [ "root = x + y"
                          , "a = b * c"
                          , "c = d -> a" ]

      runParser "recursive3" program3 `shouldBe` expectedError "c"

      let program4 = wrap [ "root = a + y"
                          , "d = x + root"
                          , "a = b * c"
                          , "c = d -> e" ]

      runParser "recursive4" program4 `shouldBe` expectedError "c"
