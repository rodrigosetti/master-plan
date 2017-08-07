{-# LANGUAGE UnicodeSyntax #-}
module MasterPlan.ParserSpec where

import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as M
import           MasterPlan.Arbitrary        ()
import           MasterPlan.Backend.Identity (render)
import           MasterPlan.Data
import           MasterPlan.Parser           (runParser)
import           Test.Hspec
import           Test.QuickCheck

spec ∷ Spec
spec =
  describe "parser" $

    it "identity backend output should parse into the same input" $ do

      let propertyParseAndOutputIdentity ∷ ProjectSystem → Property
          propertyParseAndOutputIdentity sys =
            let sys' = sys { bindings = M.map simplify $ bindings sys}
            in runParser "test" (render sys') === Right sys'

      property $ withMaxSuccess 50 propertyParseAndOutputIdentity
