module Main (main) where

import Control.Monad (unless)
import Control.Monad.State (execState)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.IO (isEOF)

import Options.Applicative

import qualified StatefulScanner as SS

data Command
    = RunScannerREPL
    | RunParserREPL
    | RunFile FilePath
    deriving (Show)

runFile :: FilePath -> IO ()
runFile fname = do
    contents <- readFile fname
    let result = execState SS.scanTokens (SS.ScannerState (T.pack contents) 0 0 1 [] [])
    print $ SS.tokens result

scannerPrompt :: IO ()
scannerPrompt = go
  where
    go = do
        putStr "> "
        ended <- isEOF
        unless
            ended
            ( do
                input <- getLine
                unless (null input) $ do
                    let result = execState SS.scanTokens (SS.ScannerState (T.pack input) 0 0 1 [] [])
                    print $ SS.tokens result
                go
            )

parserPrompt :: IO ()
parserPrompt = putStrLn "Parser not yet implemented."

scannerP :: Parser Command
scannerP =
    flag'
        RunScannerREPL
        ( long "scanner"
            <> short 's'
            <> help "Run the interactive scanner REPL"
        )

parserP :: Parser Command
parserP =
    flag'
        RunParserREPL
        ( long "parser"
            <> short 'p'
            <> help "Run the interactive parser REPL"
        )

fileP :: Parser Command
fileP =
    RunFile
        <$> strArgument
            ( metavar "FILE"
                <> help "The lox file to run"
            )

commandParser :: Parser Command
commandParser = scannerP <|> parserP <|> fileP

programOptions :: Parser Command
programOptions = fromMaybe RunScannerREPL <$> optional commandParser

main :: IO ()
main = do
    selectedCommand <- execParser opts
    case selectedCommand of
        RunScannerREPL -> scannerPrompt
        RunParserREPL -> parserPrompt
        RunFile path -> runFile path
  where
    opts =
        info
            (programOptions <**> helper)
            ( fullDesc
                <> progDesc "Runs the Lox interpreter. Defaults to an interactive scanner REPL."
                <> header "lox - A Lox interpreter in Haskell"
            )
