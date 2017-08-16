{-|
Module      : MasterPlan.Parser
Description : export parser for project systems
Copyright   : (c) Rodrigo Setti, 2017
License     : MIT
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module MasterPlan.Parser (runParser) where

import           Control.Applicative        (empty)
import           Control.Monad.State
import           Data.Generics              hiding (empty)
import           Data.List                  (nub)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe, isJust)
import qualified Data.Text                  as T
import           Data.Void
import           MasterPlan.Data
import           Text.Megaparsec            hiding (State, runParser)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

type Parser = Parsec Void T.Text

-- |Space consumer
sc ∷ Parser ()
sc = L.space space1 (L.skipLineComment ">") empty

lexeme ∷ Parser a → Parser a
lexeme = L.lexeme sc

symbol ∷ T.Text → Parser T.Text
symbol = L.symbol sc

-- | 'parens' parses something between parenthesis.
parens ∷ Parser a → Parser a
parens = between (symbol "(") (symbol ")")

-- |list of reserved words
rws ∷ [String]
rws = map show [minBound :: ProjAttribute ..]

identifier ∷ Parser String
identifier = (lexeme . try) $ (:) <$> letterChar <*> many alphaNumChar

projectKey :: Parser ProjectKey
projectKey = ProjectKey <$> (identifier >>= check) <?> "project key"
  where
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

expression ∷ Parser ProjectExpr
expression =
    simplifyProj <$> makeExprParser term table <?> "expression"
  where
    term = parens expression <|> (Reference <$> projectKey)
    table = [[binary "*" (combineWith Product)]
            ,[binary "->" (combineWith Sequence)]
            ,[binary "+" (combineWith Sum)]]
    binary  op f = InfixL  (f <$ symbol op)

    combineWith :: (NE.NonEmpty ProjectExpr -> ProjectExpr) -> ProjectExpr -> ProjectExpr -> ProjectExpr
    combineWith c p1 p2 = c $ p1 NE.<| [p2]


binding :: ProjectKey -> Parser Binding
--binding key = do (props, mc, mt, mp) <- bracketAttributes
binding key = do (props, mc, mt, mp) <- try simpleTitle <|> try bracketAttributes <|> noAttributes
                 case (mc, mt, mp) of
                  (Nothing, Nothing, Nothing) ->
                     try (BindingExpr props <$> (sc *> optional (symbol "=") *> expression)) <|>
                       pure (BindingAtomic props defaultCost defaultTrust defaultProgress)
                  (mc', mt', mp') -> pure $ BindingAtomic props
                                                         (fromMaybe defaultCost mc')
                                                         (fromMaybe defaultTrust mt')
                                                         (fromMaybe defaultProgress mp')
 where
   attrKey :: Parser ProjAttribute
   attrKey = do n <- identifier <?> "attribute name"
                case lookup n [(show a, a) | a <- [minBound::ProjAttribute ..]] of
                  Nothing -> fail $ "invalid attribute: \"" ++ n ++ "\""
                  Just a  -> pure a

   simpleTitle, bracketAttributes, noAttributes :: Parser (ProjectProperties, Maybe Cost, Maybe Trust, Maybe Progress)
   simpleTitle = do s <- stringLiteral <?> "title"
                    pure (defaultProjectProps {title=s}, Nothing, Nothing, Nothing)

   bracketAttributes = symbol "{" *> attributes (defaultProjectProps {title=getProjectKey key}) Nothing Nothing Nothing

   noAttributes = pure (defaultProjectProps {title=getProjectKey key}, Nothing, Nothing, Nothing)

   attributes :: ProjectProperties -> Maybe Cost -> Maybe Trust -> Maybe Progress
              -> Parser (ProjectProperties, Maybe Cost, Maybe Trust, Maybe Progress)
   attributes props mc mt mp =
     try (sc *> symbol "}" *> pure (props, mc, mt, mp)) <|>
         do attr <- sc *> attrKey
            case attr of
              PTitle -> do s <- stringLiteral <?> "title"
                           attributes (props {title=s}) mc mt mp
              PDescription -> do when (isJust $ description props) $ fail "redefinition of description"
                                 s <- stringLiteral <?> "description"
                                 attributes (props {description=Just s}) mc mt mp
              PUrl -> do when (isJust $ url props) $ fail "redefinition of url"
                         s <- stringLiteral <?> "url"
                         attributes (props {url=Just s}) mc mt mp
              POwner -> do when (isJust $ owner props) $ fail "redefinition of owner"
                           s <- stringLiteral <?> "owner"
                           attributes (props {owner=Just s}) mc mt mp
              PCost -> do when (isJust mc) $ fail "redefinition of cost"
                          c <- Cost <$> nonNegativeNumber <?> "cost"
                          attributes props (Just c) mt mp
              PTrust -> do when (isJust mt) $ fail "redefinition of cost"
                           t <- Trust <$> percentage <?> "trust"
                           attributes props mc (Just t) mp
              PProgress -> do when (isJust mp) $ fail "redefinition of progress"
                              p <- Progress <$> percentage <?> "progress"
                              attributes props mc mt (Just p)


-- find out all the names that a particular binding references
dependencies ∷ ProjectSystem -> Binding → [ProjectKey]
dependencies sys = everything (++) ([] `mkQ` collectDep)
  where
    collectDep (Reference n) = nub $ n : everything (++) ([] `mkQ` collectDep) (M.lookup n $ bindings sys)
    collectDep _ = []

projectSystem :: Parser ProjectSystem
projectSystem =
    mkProjSystem <$> definitions []
 where
   mkProjSystem = ProjectSystem . M.fromList

   definitions ds = do key <- sc *> projectKey
                       when (key `elem` map fst ds) $ fail $ "redefinition of \"" ++ getProjectKey key ++ "\""
                       b <- binding key <* symbol ";"

                       -- check if it's recursive
                       let deps = dependencies (mkProjSystem ds) b
                       when (key `elem` deps) $ fail $ "definition of \"" ++ getProjectKey key ++ "\" is recursive"

                       let ds' = (key,b):ds
                       (try eof *> pure ds') <|> definitions ds'

runParser :: FilePath -> T.Text -> Either String ProjectSystem
runParser filename contents = case parse projectSystem filename contents of
                                Left e -> Left $ parseErrorPretty' contents e
                                Right v -> Right v
