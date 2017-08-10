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

import           Data.List                   (intercalate)
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as M
import           Data.Maybe                  (catMaybes, fromMaybe)
import           Data.Semigroup              ((<>))
import qualified MasterPlan.Backend.Graph    as BG
import qualified MasterPlan.Backend.Identity as BI
import qualified MasterPlan.Backend.Text     as BT
import           MasterPlan.Data
import qualified MasterPlan.Parser           as P
import           Options.Applicative
import           System.IO                   (hPutStr, stderr)

-- |Type output from the command line parser
data Opts = Opts { inputPath  :: FilePath
                 , outputPath :: Maybe FilePath
                 , projFilter :: ProjFilter -- ^ filter to consider
                 , properties :: [ProjProperty] -- ^ which properties to consider
                 , prioritize :: Bool -- ^ order by priority
                 , renderMode :: RenderMode }
  deriving (Show)

newtype ProjFilter = ProjFilter (ProjectBinding → Bool)

noFilter ∷ ProjFilter
noFilter = ProjFilter $ const True

instance Show ProjFilter where
  show _ = "ProjFilter"

data RenderMode = IdentityRenderMode | TextRenderMode | GraphRenderMode
  deriving (Eq, Show)

readEnum ∷ [(String, a)] → ReadM a
readEnum mapping = maybeReader $ flip lookup mapping

-- |The command line parser
cmdParser ∷ Parser Opts
cmdParser = Opts <$> strOption ( long "input"
                               <> short 'i'
                               <> help "plan file to read from"
                               <> value "master.plan"
                               <> showDefault
                               <> metavar "FILENAME" )
                 <*> optional (strOption ( long "output"
                                         <> short 'o'
                                         <> help "output file name"
                                         <> metavar "FILENAME" ))
                 <*> (filterParser <|> pure noFilter)
                 <*> (invertProps <$> many (option property ( long "hide"
                                                            <> help "hide a particular property"
                                                            <> metavar (intercalate "|" $ map fst propertyNames))))
                 <*> switch ( long "prioritize"
                            <> short 'p'
                            <> help "prioritize projects to minimize cost")
                 <*> option parseRenderMode ( long "mode"
                                            <> short 'm'
                                            <> help "render mode"
                                            <> metavar (intercalate "|" $ map fst nameRenderModes))
  where
    propertyNames = map (\p -> (show p, p)) [minBound :: ProjProperty ..]
    nameRenderModes = [ ("identity", IdentityRenderMode)
                      , ("text", TextRenderMode)
                      , ("graph", GraphRenderMode) ]
    property = readEnum propertyNames
    parseRenderMode = readEnum nameRenderModes

    invertProps ∷ [ProjProperty] → [ProjProperty]
    invertProps l = filter (`notElem` l) $ map snd propertyNames

    filterParser ∷ Parser ProjFilter
    filterParser = (ProjFilter . mkProgressFilter) <$> option auto ( long "progress-below"
                                                                   <> help "only display projects which progress is < N%"
                                                                   <> metavar "N" )
      where
        mkProgressFilter n (TaskProj _ _ _ p) = p*100 < n
        mkProgressFilter _ _                  = True

main ∷ IO ()
main = masterPlan =<< execParser opts
  where
    opts = info (cmdParser <**> helper)
      ( fullDesc
     <> progDesc "See documentation on how to write project plan files"
     <> header "master-plan - project management tool for hackers" )

filterBinding ∷ ProjectSystem → ProjFilter → ProjectBinding → Maybe ProjectBinding
filterBinding sys (ProjFilter f) (ExpressionProj r p) = ExpressionProj r <$> filterProj p
  where
  filterProj (SumProj ps) = SumProj <$> filterProjs ps
  filterProj (ProductProj ps) = ProductProj <$> filterProjs ps
  filterProj (SequenceProj ps) = SequenceProj <$> filterProjs ps
  filterProj rp@(RefProj n) = do b <- M.lookup n $ bindings sys
                                 if f b then Just rp else Nothing

  filterProjs ps = NE.nonEmpty (catMaybes $ NE.toList $ NE.map filterProj ps)
filterBinding _   _ b = Just b

masterPlan ∷ Opts → IO ()
masterPlan opts =
    do contents <- readFile filename
       case P.runParser filename contents of
          Left e    -> hPutStr stderr e
          Right sys@(ProjectSystem b) ->
            render $ maybeOptimize $ ProjectSystem $ M.mapMaybe
                                                    (filterBinding sys $ projFilter opts) b
  where
    filename = inputPath opts

    maybeOptimize = if prioritize opts then optimizeSys else id

    outputToFileOrOut s = case outputPath opts of
                             Nothing   -> putStr s
                             Just path -> writeFile path s

    render sys =
      case renderMode opts of
        IdentityRenderMode -> outputToFileOrOut $ BI.render sys $ properties opts
        TextRenderMode -> outputToFileOrOut $ BT.render sys $ properties opts
        GraphRenderMode -> do let outfile = fromMaybe (filename ++ ".png") $ outputPath opts
                              BG.render outfile sys $ properties opts
