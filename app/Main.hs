{-|
Module      : Main
Description : Parses command line and dispatches to correct backend
Copyright   : (c) Rodrigo Setti, 2017
License     : MIT
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import           Data.List                (intercalate)
import qualified Data.List.NonEmpty       as NE
import           Data.Maybe               (catMaybes, fromMaybe)
import           Data.Semigroup           ((<>))
import qualified Data.Text.IO             as TIO
import           MasterPlan.Backend.Graph
import           MasterPlan.Data
import qualified MasterPlan.Parser        as P
import           Options.Applicative
import           System.Exit              (die)
import           System.IO                (stdin)

-- |Type output from the command line parser
data Opts = Opts { inputPath          :: Maybe FilePath
                 , outputPath         :: Maybe FilePath
                 , rootKey            :: ProjectKey -- ^ name of the root project
                 , projFilter         :: ProjFilter -- ^ filter to consider
                 , renderParsingError :: Bool -- ^ will render the parsing error instead of printing
                 , parseStrict        :: Bool -- ^ every project has to be defined
                 , renderOptions      :: RenderOptions }

type ProjFilter = ProjectExpr → Bool

noFilter ∷ ProjFilter
noFilter = const True

readEnum ∷ [(String, a)] → ReadM a
readEnum mapping = maybeReader $ flip lookup mapping

-- |The command line parser
cmdParser ∷ Parser Opts
cmdParser = Opts <$> optional (strArgument ( help "plan file to read from (default from stdin)"
                                           <> metavar "FILENAME" ))
                 <*> optional (strOption ( long "output"
                                         <> short 'o'
                                         <> help "output file name (.png, .tif, .bmp, .jpg and .pdf supported)"
                                         <> metavar "FILENAME" ))
                 <*> strOption ( long "root"
                               <> short 'r'
                               <> help "name of the root project definition"
                               <> value "root"
                               <> showDefault
                               <> metavar "NAME")
                 <*> (filterParser <|> pure noFilter)
                 <*> switch ( long "render-parse-error"
                            <> help "instead of printing parsing errors, render as an image")
                 <*> switch ( long "strict"
                            <> help "strict parsing: every project has to be defined")
                 <*> renderOptionsParser
  where
    renderOptionsParser ∷ Parser RenderOptions
    renderOptionsParser = RenderOptions <$> switch ( long "color"
                                                   <> short 'c'
                                                   <> help "color each project by progress")
                                        <*> option auto ( long "width"
                                                         <> short 'w'
                                                         <> help "width of the output image"
                                                         <> value (-1)
                                                         <> metavar "NUMBER")
                                        <*> option auto ( long "height"
                                                         <> help "height of the output image"
                                                         <> value (-1)
                                                         <> metavar "NUMBER")
                                        <*> (invertProps <$> many (option property ( long "hide"
                                                                                   <> help "hide a particular property"
                                                                                   <> metavar (intercalate "|" $ map fst propertyNames))))
    propertyNames = map (\p -> (show p, p)) [minBound :: ProjAttribute ..]
    property = readEnum propertyNames

    invertProps ∷ [ProjAttribute] → [ProjAttribute]
    invertProps l = filter (`notElem` l) $ map snd propertyNames

    filterParser ∷ Parser ProjFilter
    filterParser = (mkProgressFilter . Progress) <$> option auto ( long "progress-below"
                                                                              <> help "only display projects which progress is < N%"
                                                                              <> metavar "N" )
      where
        mkProgressFilter n p = progress p * 100 < n

main ∷ IO ()
main = masterPlan =<< execParser opts
  where
    opts = info (cmdParser <**> helper)
      ( fullDesc
     <> progDesc "See documentation on how to write project plan files"
     <> header "master-plan - project management tool for hackers" )

filterProj ∷ ProjFilter -> ProjectExpr → Maybe ProjectExpr
filterProj f p@(Sum r ps)      = filterHelper p f ps (Sum r)
filterProj f p@(Product r ps)  = filterHelper p f ps (Product r)
filterProj f p@(Sequence r ps) = filterHelper p f ps (Sequence r)
filterProj f p                 = if f p then Just p else Nothing

filterHelper :: ProjectExpr
             -> ProjFilter
             -> NE.NonEmpty ProjectExpr
             -> (NE.NonEmpty ProjectExpr -> ProjectExpr)
             -> Maybe ProjectExpr
filterHelper p f ps c = if f p then c <$> filterProjs ps else Nothing
  where
 filterProjs ps' = NE.nonEmpty (catMaybes $ NE.toList $ filterProj f <$> ps')

masterPlan ∷ Opts → IO ()
masterPlan opts =
    do contents <- maybe (TIO.hGetContents stdin) TIO.readFile $ inputPath opts
       let outfile = fromMaybe (fromMaybe "output" (outputPath opts) ++ ".pdf") $ outputPath opts
       case P.runParser (parseStrict opts) (fromMaybe "stdin" $ inputPath opts) contents (rootKey opts) of
          Left e    -> if renderParsingError opts
                        then renderText outfile (renderOptions opts) (lines e)
                        else die e
          Right p ->
            do let p' = fromMaybe defaultAtomic $ prioritize <$> filterProj (projFilter opts) p
               render outfile (renderOptions opts) p'
