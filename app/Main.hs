module Main (main) where

import System.Environment (getArgs)
import System.IO (isEOF)
import Control.Monad (unless)
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
      Control.Monad.unless ended (do
        input <- getLine
        putStrLn input
        go)

-- TODO: print each token on newline
-- run :: String -> IO ()
-- run xs = do
--   let scannerResults = S.scanTokens xs
--   case scannerResults of
--     tokens -> print tokens

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> runFile fname
    []      -> runPrompt
    _       -> putStrLn $ "Usage: lox [script]"

