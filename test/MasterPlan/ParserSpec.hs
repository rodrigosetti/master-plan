{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module MasterPlan.ParserSpec (spec) where

import           Data.Either                 (isRight)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import           MasterPlan.Arbitrary        ()
import           MasterPlan.Backend.Identity (render)
import           MasterPlan.Data
import           MasterPlan.Parser           (runParser)
import           Test.Hspec
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck

spec ∷ Spec
spec =
  describe "parser" $ do

    let allProps = [minBound :: ProjAttribute ..]

    let asRoot (Atomic r c t p) = Atomic r {title=Just "root"} c t p
        asRoot (Sum r ps)       = Sum r {title=Just "root"} ps
        asRoot (Sequence r ps)  = Sequence r {title=Just "root"} ps
        asRoot (Product r ps)   = Product r {title=Just "root"} ps
        asRoot p                = p

    prop "rendered should be parseable" $ do
      let renderedIsParseable ∷ ProjectExpr → Property
          renderedIsParseable p =
            let p' = simplify p
                rendered = render p' allProps
             in counterexample (T.unpack rendered) $ isRight (runParser False "test1" rendered "root")

      withMaxSuccess 50 renderedIsParseable

    prop "identity backend output should parse into the same input" $ do

      let propertyParseAndOutputIdentity ∷ ProjectExpr → Property
          propertyParseAndOutputIdentity p =
            let p' = asRoot $ simplify p
                parsed = runParser False "test2" (render p' allProps) "root"
             in isRight parsed ==> parsed === Right p'

      withMaxSuccess 50 propertyParseAndOutputIdentity

    it "should parse without prioritization" $ do

      let input = "main = a + b;\
                  \a = x + y;\
                  \b { cost 9 };\
                  \x { cost 10 };\
                  \y { cost 5 trust 90% };"

      let (Right p) = runParser True "test" input "main"

      cost p `shouldBe` 10.0

      -- now prioritize... a little out of scope for this test, but fine:

      let p' = prioritize p

      cost p' `shouldBe` 6.0

    it "should reject recursive equations" $ do

      let expectedError _   (Right _) = False
          expectedError key (Left s) =
            let l = last $ lines s
             in l == "definition of \"" ++ key ++ "\" is recursive"

      let wrap = T.unlines . map (<> ";\n")

      -- obvious
      let program1 = wrap ["root = a + b + root"]

      runParser False "recursive1" program1 "root" `shouldSatisfy` expectedError "root"

      let program2 = wrap [ "root = a + b"
                          , "a = x * root" ]

      runParser False "recursive2" program2 "root" `shouldSatisfy` expectedError "root"

      let program3 = wrap [ "xxx = x + a"
                          , "a = b * c"
                          , "c = d -> a" ]

      runParser False "recursive3" program3 "xxx" `shouldSatisfy` expectedError "a"

      let program4 = wrap [ "yyy = a + y"
                          , "d = x + yyy"
                          , "a = b * c"
                          , "c = d -> e" ]

      runParser False "recursive4" program4 "yyy" `shouldSatisfy` expectedError "yyy"
