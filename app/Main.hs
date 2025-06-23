module Main (main) where

import Control.Monad (unless)
import Control.Monad.State (execState)
import qualified Data.Text as T
import System.Environment (getArgs)
import System.IO (isEOF)

import qualified StatefulScanner as SS

runFile :: FilePath -> IO ()
runFile fname = do
    contents <- readFile fname
    let result = execState SS.scanTokens (SS.ScannerState (T.pack contents) 0 0 1 [] [])
    print $ SS.tokens result

scannerPrompt :: IO ()
scannerPrompt = go
  where
    go = do
        ended <- isEOF
        Control.Monad.unless
            ended
            ( do
                input <- getLine
                let result = execState SS.scanTokens (SS.ScannerState (T.pack input) 0 0 1 [] [])
                print $ SS.tokens result
                go
            )

parserPrompt :: IO ()
parserPrompt = undefined

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-s"] -> scannerPrompt
        ["-p"] -> parserPrompt
        [fname] -> runFile fname
        _ -> putStrLn "Usage: lox [script] | -s | -p"
