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
module MasterPlan.Parser (ProjectKey, runParser) where

import           Control.Monad.State
import qualified Data.List.NonEmpty         as NE
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
sc = L.space space1 (L.skipLineComment "//") $
                     L.skipBlockComment "/*" "*/"

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

projectKey :: Parser String
projectKey = (identifier >>= check) <?> "project key"
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

-- |Parses the part of right-hand-side after the optional properties
--  (literal string title or properties between curly brackets)
expression ∷ ProjectProperties -> Parser (Project ProjectKey)
expression props =
    simplify <$> makeExprParser term table <?> "expression"
  where
    term =  parens (expression defaultProjectProps) <|> (Annotated <$> projectKey)
    table = [[binary "*" (combineWith Product)]
            ,[binary "->" (combineWith Sequence)]
            ,[binary "+" (combineWith Sum)]]
    binary  op f = InfixL (f <$ symbol op)

    combineWith c p1 p2 = c props $ p1 NE.<| [p2]

-- |Parses the entire right-hand-side of definitions
binding :: ProjectKey -> Parser (Project ProjectKey)
binding key = do (props, mc, mt, mp) <- try simpleTitle <|> try bracketAttributes <|> noAttributes
                 case (mc, mt, mp) of
                  (Nothing, Nothing, Nothing) ->
                     try (sc *> optional (symbol "=") *> expression props) <|>
                       pure (Atomic props defaultCost defaultTrust defaultProgress)
                  (mc', mt', mp') -> pure $ Atomic props
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
                    pure (defaultProjectProps {title= Just s}, Nothing, Nothing, Nothing)

   bracketAttributes = symbol "{" *> attributes (defaultProjectProps {title=Just key}) Nothing Nothing Nothing

   noAttributes = pure (defaultProjectProps {title=Just key}, Nothing, Nothing, Nothing)

   attributes :: ProjectProperties -> Maybe Cost -> Maybe Trust -> Maybe Progress
              -> Parser (ProjectProperties, Maybe Cost, Maybe Trust, Maybe Progress)
   attributes props mc mt mp =
     try (sc *> symbol "}" *> pure (props, mc, mt, mp)) <|>
         do attr <- sc *> attrKey
            case attr of
              PTitle -> do s <- stringLiteral <?> "title"
                           attributes (props {title=Just s}) mc mt mp
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


-- |Parses the entire plan file, given the name of the "root" project.
--  returns this project
plan :: Bool -- ^ strict mode: fails if a reference has no definition
     -> ProjectKey -- ^ the name of the root project
     -> Parser ProjectExpr
plan strictMode root =
    do bindings <- definitions []
       case lookup root bindings of
         Nothing -> fail $ "root project \"" ++ root ++ "\" is undefined"
         Just p  -> resolveReferences bindings [root] p
 where

   -- definitions will parse into a list of tuples: (key, Project ProjectKey)
   definitions :: [(ProjectKey, Project ProjectKey)] -> Parser [(ProjectKey, Project ProjectKey)]
   definitions ds = do key <- sc *> projectKey
                       when (key `elem` map fst ds) $ fail $ "redefinition of \"" ++ key ++ "\""
                       b <- binding key <* symbol ";"
                       let ds' = (key,b):ds
                       (try eof *> pure ds') <|> definitions ds'

   resolveReferences :: [(ProjectKey, Project ProjectKey)]
                     -> [ProjectKey]
                     -> Project ProjectKey
                     -> Parser ProjectExpr
   resolveReferences bs ks (Sum r ps) = Sum r <$> mapM (resolveReferences bs ks) ps
   resolveReferences bs ks (Sequence r ps) = Sequence r <$> mapM (resolveReferences bs ks) ps
   resolveReferences bs ks (Product r ps) = Product r <$> mapM (resolveReferences bs ks) ps
   resolveReferences bs ks (Annotated k)
      | k `elem` ks  = fail $ "definition of \"" ++ k ++ "\" is recursive"
      | otherwise = case lookup k bs of
                      Nothing
                       | strictMode -> fail $ "project \"" ++ k ++ "\" is undefined"
                       | otherwise -> pure $ Atomic defaultProjectProps {title=Just k}
                                                    defaultCost defaultTrust defaultProgress
                      Just p  -> resolveReferences bs (k:ks) p
   resolveReferences _ _ (Atomic r c t p) = pure $ Atomic r c t p

runParser :: Bool -- ^ strict mode: fails if a reference has no definition
          -> FilePath -- ^ the file name to use in error messages
          -> T.Text -- ^ contents of the plan to parse
          -> ProjectKey -- ^ the name of the root project
          -> Either String ProjectExpr
runParser strictMode filename contents root =
  case parse (plan strictMode root) filename contents of
      Left e  -> Left $ parseErrorPretty' contents e
      Right v -> Right v
