{-|
Module      : MasterPlan.Parser
Description : export parser for project systems
Copyright   : (c) Rodrigo Setti, 2017
License     : MIT
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module MasterPlan.Parser (runParser) where

import           Control.Monad.State
import           Data.Generics
import           Data.List                  (nub)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Data.Void
import           MasterPlan.Data
import           Text.Megaparsec            hiding (State, runParser)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

type Parser = ParsecT Void T.Text (State ProjectSystem)

-- |Space consumer
sc ∷ Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme ∷ Parser a → Parser a
lexeme = L.lexeme sc

symbol ∷ T.Text → Parser T.Text
symbol = L.symbol sc

-- | 'parens' parses something between parenthesis.
parens ∷ Parser a → Parser a
parens = between (symbol "(") (symbol ")")

-- |list of reserved words
rws ∷ [String]
rws = map show [minBound :: ProjProperty ..]

identifier ∷ Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x
      | x `elem` rws = fail $ "keyword " ++ show x ++ " cannot be an identifier"
      | otherwise    = pure x

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

percentage :: Parser Float
percentage = do n <- L.float <?> "percentage value"
                when (n > 100) $ fail $ "number " ++ show n ++ " is not within (0,100) range"
                void $ symbol "%"
                pure $ n / 100

nonNegativeNumber :: Parser Float
nonNegativeNumber = L.float

definition ∷ Parser ()
definition =
    choice ([ propsProp PTitle stringLiteral (\v p -> p { title = v })
            , propsProp PDescription stringLiteral (\v p -> p { description = Just v})
            , propsProp PUrl stringLiteral (\v p -> p { url = Just v})
            , propsProp POwner stringLiteral (\v p -> p { owner = Just v})
            , taskProp  PCost nonNegativeNumber (\v b -> case b of TaskProj r _ t p -> TaskProj r v t p; _ -> b)
            , taskProp  PTrust percentage (\v b -> case b of TaskProj r c _ p -> TaskProj r c v p; _ -> b)
            , taskProp  PProgress percentage (\v b -> case b of TaskProj r c t _ -> TaskProj r c t v; _ -> b)
            , structure ] :: [Parser ()])
  where
    structure :: Parser ()
    structure = do projName <- identifier
                   projectExpr <- symbol "=" *> expressionParser
                   sys <- lift get

                   -- check if it's recursive
                   let deps = dependencies sys projectExpr
                   when (projName `elem` deps) $ fail $ "definition of \"" ++ projName ++ "\" is recursive"

                   let binding = M.lookup projName $ bindings sys
                   newBinding <- case binding of
                                   Nothing -> pure $ ExpressionProj (defaultProjectProps { title=projName }) projectExpr
                                   Just ExpressionProj {} -> fail $ "Redefinition of \"" ++ projName ++ "\"."
                                   Just (UnconsolidatedProj p) -> pure $ ExpressionProj p projectExpr
                                   Just TaskProj {} -> fail $ "Project \"" ++ projName ++ "\" is atomic"

                   lift $ put $ sys { bindings = M.insert projName newBinding $ bindings sys }

    propsProp :: ProjProperty -> Parser a -> (a -> ProjectProperties -> ProjectProperties) -> Parser ()
    propsProp prop valueParser modifier =
       property prop valueParser setter
     where
       setter projName val Nothing = pure $ UnconsolidatedProj $ modifier val $ defaultProjectProps { title=projName }
       setter _ val (Just p) = pure $ everywhere (mkT $ modifier val) p

    taskProp :: ProjProperty -> Parser a -> (a -> ProjectBinding -> ProjectBinding) -> Parser ()
    taskProp prop valueParser modifier =
       property prop valueParser setter
     where
       setter projName val Nothing = pure $ modifier val $ defaultTaskProj defaultProjectProps { title=projName }
       setter projName _ (Just ExpressionProj {}) = fail $ "Project \"" ++ projName ++ "\" is not atomic."
       setter _ val (Just (UnconsolidatedProj p)) = pure $ modifier val $ defaultTaskProj p
       setter _ val (Just p@TaskProj {}) = pure $ modifier val p

    property ∷ ProjProperty → Parser a → (String -> a -> Maybe ProjectBinding -> Parser ProjectBinding) -> Parser ()
    property prop valueParser setter =
       do void $ symbol $ T.pack $ show prop
          projName <- parens identifier
          mBinding <- lift $ M.lookup projName <$> gets bindings
          value <- symbol "=" *> valueParser
          newBinding <- setter projName value mBinding
          let modifySys :: ProjectSystem -> ProjectSystem
              modifySys sys = sys { bindings = M.insert projName newBinding $ bindings sys }
          lift $ modify modifySys

expressionParser ∷ Parser Project
expressionParser =
    simplifyProj <$> makeExprParser term table <?> "expression"
  where
    term = parens expressionParser <|> (RefProj <$> identifier)
    table = [[binary "*" (combineWith ProductProj)]
            ,[binary "->" (combineWith SequenceProj)]
            ,[binary "+" (combineWith SumProj)]]
    binary  op f = InfixL  (f <$ symbol op)

    combineWith :: (NE.NonEmpty Project -> Project) -> Project -> Project -> Project
    combineWith c p1 p2 = c $ p1 NE.<| [p2]

dependencies ∷ ProjectSystem -> Project → [ProjectKey]
dependencies sys = everything (++) ([] `mkQ` collectDep)
  where
    collectDep (RefProj n) = nub $ n : everything (++) ([] `mkQ` collectDep) (M.lookup n $ bindings sys)
    collectDep _ = []

projectSystem :: Parser ProjectSystem
projectSystem =
    do between sc eof definitionSeq
       ps <- lift get
       unless (M.member rootKey $ bindings ps) $ fail $ "expected project \"" ++ rootKey ++ "\" to be defined."
       pure ps
 where
   definitionSeq = void $ endBy1 definition (symbol ";")


runParser :: FilePath -> T.Text -> Either String ProjectSystem
runParser filename contents = let mr = runParserT projectSystem filename contents
                                  initialPS = ProjectSystem M.empty
                              in case evalState mr initialPS of
                                    Left e -> Left $ parseErrorPretty' contents e
                                    Right v -> Right v
