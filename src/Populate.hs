module Populate
    ( run
    ) where

import qualified Data.Text.IO as T
import Options.Applicative
import System.Environment (getArgs)
import System.IO (FilePath)

import Populate.Sources


-- | Parse out a filepath and whether to overwrite
args :: Parser (String, Bool)
args = (,)
    <$> strArgument (metavar "FILE")
    <*> switch
        (  long "overwrite"
        <> short 'w'
        <> help "Whether to overwrite cached files"
        )

-- | The entry point for the application.
run :: IO ()
run = execParser opts >>= runOn
  where
    opts = info (args <**> helper)
        ( fullDesc
        <> progDesc "Populate based on the songs in FILE"
        )


-- | Runs the program with a given file
runOn :: (FilePath, Bool) -> IO ()
runOn (file, overwrite) = do
    contents <- T.readFile file    
    let parseRes = parseSources contents file
    case parseRes of
        Left err      -> T.putStrLn (prettyProgramError err)
        Right sources -> downloadSources overwrite sources
