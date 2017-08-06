{-|
Module      : MasterPlan.Parser
Description : export parser for project systems
Copyright   : (c) Rodrigo Setti, 2017
License     : MIT
Maintainer  : rodrigosetti@email.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedLists #-}
module MasterPlan.Parser (runParser) where

import           Control.Monad.State
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import           Data.Semigroup             ((<>))
import           Data.Void
import           MasterPlan.Data
import           Text.Megaparsec            hiding (State, runParser)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

{-
* Grammar

definition = project_def | predicate_def

project_def = identifier "=" expression
expression = term ((">>" | "x") term)*
term = factor ("+" factor)*
factor = "(" expression ")" | identifier

predicate_def = identifier "(" identifier ")" "=" value
value = number | text
-}

type Parser = ParsecT Void String (State ProjectSystem)

-- |Space consumer
sc ∷ Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme ∷ Parser a → Parser a
lexeme = L.lexeme sc

symbol ∷ String → Parser String
symbol = L.symbol sc

-- | 'parens' parses something between parenthesis.
parens ∷ Parser a → Parser a
parens = between (symbol "(") (symbol ")")

-- |list of reserved words
rws ∷ [String]
rws = ["name", "description", "url", "owner", "status", "progress", "cost", "risk"]

identifier ∷ Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

statusParser :: Parser Status
statusParser = choice ([ symbol "ready" *> pure Ready
                       , symbol "blocked" *> pure Blocked
                       , symbol "progress" *> pure Progress
                       , symbol "done" *> pure Done
                       , symbol "cancelled" *> pure Cancelled ] :: [Parser Status])

percentage :: Parser Float
percentage = do n <- L.float <* symbol "%"
                pure $ n / 100

nonNegativeNumber :: Parser Float
nonNegativeNumber = L.float

definition ∷ Parser ()
definition =
    choice ([ propsProp "name" stringLiteral (\v p -> p { title = v })
            , propsProp "description" stringLiteral (\v p -> p { description = Just v})
            , propsProp "url" stringLiteral (\v p -> p { url = Just v})
            , propsProp "owner" stringLiteral (\v p -> p { owner = Just v})
            , taskProp "status" statusParser (\v b -> case b of t@TaskProj {} -> t { reportedStatus = v }; _ -> b)
            , taskProp "progress" percentage (\v b -> case b of t@TaskProj {} -> t { reportedProgress = v }; _ -> b)
            , taskProp "cost" nonNegativeNumber (\v b -> case b of t@TaskProj {} -> t { reportedCost = v }; _ -> b)
            , taskProp "trust" percentage (\v b -> case b of t@TaskProj {} -> t { reportedTrust = v }; _ -> b)
            , structure ] :: [Parser ()])
  where
    structure :: Parser ()
    structure = do projName <- identifier
                   projectExpr <- symbol "=" *> expressionParser
                   binding <- lift $ gets $ M.lookup projName . bindings
                   newBinding <- case binding of
                                   Nothing -> pure $ ExpressionProj (defaultProjectProps { title=projName }) projectExpr
                                   Just ExpressionProj {} -> fail $ "Redefinition of \"" ++ projName ++ "\"."
                                   Just (UnconsolidatedProj p) -> pure $ ExpressionProj p projectExpr
                                   Just TaskProj {} -> fail $ "Project \"" ++ projName ++ "\" is atomic"
                   lift $ modify (\sys -> sys { bindings = M.insert projName newBinding $ bindings sys })

    propsProp :: String -> Parser a -> (a -> ProjectProperties -> ProjectProperties) -> Parser ()
    propsProp propName valueParser modifier =
       property propName valueParser setter
     where
       setter projName val Nothing = pure $ UnconsolidatedProj $ modifier val $ defaultProjectProps { title=projName }
       setter _ val (Just p) = pure $ p { props = modifier val $ props p}

    taskProp :: String -> Parser a -> (a -> ProjectBinding -> ProjectBinding) -> Parser ()
    taskProp propName valueParser modifier =
       property propName valueParser setter
     where
       setter projName val Nothing = pure $ modifier val $ defaultTaskProj { props = defaultProjectProps { title=projName }}
       setter projName _ (Just ExpressionProj {}) = fail $ "Project \"" ++ projName ++ "\" is not atomic."
       setter _ val (Just (UnconsolidatedProj p)) = pure $ modifier val $ defaultTaskProj { props=p }
       setter _ val (Just p@TaskProj {}) = pure $ modifier val p

    property ∷ String → Parser a → (String -> a -> Maybe ProjectBinding -> Parser ProjectBinding) -> Parser ()
    property propName valueParser setter = do _ <- symbol propName
                                              projName <- parens identifier
                                              mBinding <- lift $ do b <- gets bindings
                                                                    pure $ M.lookup projName b
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
    table = [[binary "*" combineProduct]
            ,[binary "->" combineSequence]
            ,[binary "+" combineSum]]
    binary  op f = InfixL  (f <$ symbol op)

    combineProduct ∷ Project → Project → Project
    combineProduct p1 p2 = ProductProj $ p1 NE.<| [p2]
    --combineProduct (ProductProj ps1) (ProductProj ps2) = ProductProj $ ps1 <> ps2
    --combineProduct (ProductProj ps) p = ProductProj $ ps <> [p]
    --combineProduct p (ProductProj ps) = ProductProj $ p NE.<| ps
    --combineProduct p1 p2 = ProductProj [p1, p2]

    combineSequence ∷ Project → Project → Project
    combineSequence p1 p2 = SequenceProj $ p1 NE.<| [p2]
    --combineSequence (SequenceProj ps1) (SequenceProj ps2) = SequenceProj $ ps1 <> ps2
    --combineSequence (SequenceProj ps) p = SequenceProj $ ps <> [p]
    --combineSequence p (SequenceProj ps) = SequenceProj $ p NE.<| ps
    --combineSequence p1 p2 = SequenceProj [p1, p2]

    combineSum ∷ Project → Project → Project
    combineSum p1 p2 = SumProj $ p1 NE.<| [p2]
    --combineSum (SumProj ps1) (SumProj ps2) = SumProj $ ps1 <> ps2
    --combineSum (SumProj ps) p              = SumProj $ ps <> [p]
    --combineSum p (SumProj ps)              = SumProj $ p NE.<| ps
    --combineSum p1 p2                       = SumProj [p1, p2]


projectSystem :: Parser ProjectSystem
projectSystem =
    do between sc eof definitionSeq
       ps <- lift get
       unless (M.member "root" $ bindings ps) $ fail "expected project \"root\" to be defined."
       pure ps
 where
   definitionSeq = void $ endBy1 definition (symbol ";")


runParser :: String -> String -> Either String ProjectSystem
runParser filename contents = let mr = runParserT projectSystem filename contents
                                  initialPS = ProjectSystem M.empty
                              in case evalState mr initialPS of
                                    Left e -> Left $ parseErrorTextPretty e
                                    Right v -> Right v
