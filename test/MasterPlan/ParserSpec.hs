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
