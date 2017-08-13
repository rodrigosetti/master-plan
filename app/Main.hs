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
import qualified Data.Map                 as M
import           Data.Maybe               (catMaybes, fromMaybe)
import           Data.Semigroup           ((<>))
import qualified Data.Text.IO             as TIO
import           MasterPlan.Backend.Graph
import           MasterPlan.Data
import qualified MasterPlan.Parser        as P
import           Options.Applicative
import           System.IO                (hPutStr, stderr, stdin)

-- |Type output from the command line parser
data Opts = Opts { inputPath     :: Maybe FilePath
                 , outputPath    :: Maybe FilePath
                 , projFilter    :: ProjFilter -- ^ filter to consider
                 , renderOptions :: RenderOptions }
  deriving (Show)

newtype ProjFilter = ProjFilter (ProjectSystem → ProjectExpr → Bool)

noFilter ∷ ProjFilter
noFilter = ProjFilter $ const $ const True

instance Show ProjFilter where
  show _ = "ProjFilter"

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
                 <*> (filterParser <|> pure noFilter)
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
                                        <*> strOption ( long "root"
                                                       <> short 'r'
                                                       <> help "name of the root project definition"
                                                       <> value "root"
                                                       <> showDefault
                                                       <> metavar "NAME")
                                        <*> (invertProps <$> many (option property ( long "hide"
                                                                                   <> help "hide a particular property"
                                                                                   <> metavar (intercalate "|" $ map fst propertyNames))))
    propertyNames = map (\p -> (show p, p)) [minBound :: ProjProperty ..]
    property = readEnum propertyNames

    invertProps ∷ [ProjProperty] → [ProjProperty]
    invertProps l = filter (`notElem` l) $ map snd propertyNames

    filterParser ∷ Parser ProjFilter
    filterParser = (ProjFilter . mkProgressFilter) <$> option auto ( long "progress-below"
                                                                   <> help "only display projects which progress is < N%"
                                                                   <> metavar "N" )
      where
        mkProgressFilter n sys p = progress sys p * 100 < n

main ∷ IO ()
main = masterPlan =<< execParser opts
  where
    opts = info (cmdParser <**> helper)
      ( fullDesc
     <> progDesc "See documentation on how to write project plan files"
     <> header "master-plan - project management tool for hackers" )

filterBinding ∷ ProjectSystem → ProjFilter → Binding → Maybe Binding
filterBinding sys (ProjFilter f) (BindingExpr r e) = BindingExpr r <$> filterProj e
  where
  filterProj p@(Sum ps)      = filterHelper p ps Sum
  filterProj p@(Product ps)  = filterHelper p ps Product
  filterProj p@(Sequence ps) = filterHelper p ps Sequence
  filterProj p                   = if f sys p then Just p else Nothing

  filterHelper p ps c = if f sys p then c <$> filterProjs ps else Nothing
  filterProjs ps = NE.nonEmpty (catMaybes $ NE.toList $ filterProj <$> ps)

filterBinding _   _ b = Just b

masterPlan ∷ Opts → IO ()
masterPlan opts =
    do contents <- maybe (TIO.hGetContents stdin) TIO.readFile $ inputPath opts
       case P.runParser (fromMaybe "stdin" $ inputPath opts) contents of
          Left e    -> hPutStr stderr e
          Right sys@(ProjectSystem b) ->
            do let sys' = prioritizeSys $ ProjectSystem $ M.mapMaybe
                                                    (filterBinding sys $ projFilter opts) b
               let outfile = fromMaybe (fromMaybe "output" (outputPath opts) ++ ".pdf") $ outputPath opts
               render outfile (renderOptions opts) sys'
