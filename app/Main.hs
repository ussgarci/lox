module Main (main) where

import System.Environment (getArgs)
import System.IO (isEOF)
import Scanner
import qualified MegaScanner as MT


runFile :: FilePath -> IO ()
runFile fname = do
  f <- readFile fname
  result <- MT.scan f
  putStrLn (show result)

runPrompt :: IO ()
runPrompt = go 
  where 
    go = do
      ended <- isEOF
      case ended of
        True -> return ()
        False -> do
          input <- getLine
          putStrLn input
          go

run :: String -> IO ()
run xs = do 
  let scannerResults = scanTokens xs
  case scannerResults of 
    tokens -> putStrLn (show tokens) 
    -- TODO: print each token on newline

main :: IO ()
main = do
  args <- getArgs 
  case args of 
    [fname] -> runFile fname
    []      -> runPrompt
    _       -> putStrLn $ "Usage: lox [script]"

