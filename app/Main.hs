{-# LANGUAGE UnicodeSyntax #-}
module Main where

import           Data.Semigroup              ((<>))
import           MasterPlan.Backend.Identity
import           MasterPlan.Data
import qualified MasterPlan.Parser           as P
import           Options.Applicative

-- |Type output from the command line parser
data Opts = Opts { inputPath  :: FilePath
                 , outputPath :: Maybe FilePath
                 , filters    :: [ProjFilter] -- ^ set of filters to consider
                 , properties :: [ProjProperty] -- ^ which properties to consider
                 , prioritize :: Bool -- ^ order by priority
                 , renderMode :: RenderMode }
  deriving (Show)

data ProjProperty = PTitle | PDescription | PUrl | POwner | PCost | PTrust | PProgress
  deriving (Eq, Show)

newtype ProjFilter = ProjFilter (ProjectBinding → Bool)

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
                 <*> many filterParser
                 <*> (invertProps <$> many (option property ( long "hide"
                                                            <> help "hide a particular property"
                                                            <> metavar "PROPERTY")))
                 <*> switch ( long "prioritize"
                            <> short 'p'
                            <> help "prioritize projects to minimize cost")
                 <*> option parseRenderMode ( long "mode"
                                            <> short 'm'
                                            <> help "render mode"
                                            <> metavar "identity|text|graph")
  where
    nameProps = [ ("title", PTitle)
                , ("description", PDescription)
                , ("url", PUrl)
                , ("owner", POwner)
                , ("cost", PCost)
                , ("trust", PTrust)
                , ("progress", PProgress) ]
    property = readEnum nameProps
    parseRenderMode = readEnum [ ("identity", IdentityRenderMode)
                               , ("text", TextRenderMode)
                               , ("graph", GraphRenderMode) ]

    invertProps ∷ [ProjProperty] → [ProjProperty]
    invertProps l = filter (`notElem` l) $ map snd nameProps

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

masterPlan ∷ Opts → IO ()
masterPlan opts = do let filename = inputPath opts
                     contents <- readFile filename
                     case P.runParser filename contents of
                        Left e  -> putStr e
                        Right v -> putStr $ render v
