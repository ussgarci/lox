module Main (main) where

import Control.Monad (unless)
import System.Environment (getArgs)
import System.IO (isEOF)

-- import qualified Scanner as S
import qualified MegaScanner as MT

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
        [fname] -> runFile fname
        [] -> runPrompt
        _ -> putStrLn "Usage: lox [script]"
