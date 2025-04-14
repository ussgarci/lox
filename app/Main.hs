module Main (main) where

import Control.Monad (unless)
import Control.Monad.State (evalState)
import System.Environment (getArgs)
import System.IO (isEOF)

-- import qualified Scanner as S
import qualified MegaScanner as MT
import qualified StatefulScanner as SS

runFile :: FilePath -> IO ()
runFile fname = do
    f <- readFile fname
    result <- MT.scan f
    print result

runPrompt :: IO ()
runPrompt = go
  where
    go = do
        ended <- isEOF
        Control.Monad.unless
            ended
            ( do
                input <- getLine
                putStrLn input
                go
            )

main :: IO ()
main = do
    args <- getArgs
    print args
    case args of
        [fname] -> do 
            contents <- readFile fname
            let result = evalStaAte SS.scanTokens (ScannerState contents 0 0 1) 
            print result
        [] -> runPrompt
        _ -> putStrLn "Usage: lox [script]"

-- main :: IO ()
-- main = do
--     args <- getArgs
--     print args
--     case args of
--         [fname] -> runFile fname
--         [] -> runPrompt
--         _ -> putStrLn "Usage: lox [script]"
