module Main (main) where

import System.Environment (getArgs)
import System.IO (isEOF)
import Scanner


runFile :: FilePath -> IO ()
runFile fname = do
    f <- readFile fname
    run f

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
    Just tokens -> putStrLn (show tokens) 
    Nothing     -> putStrLn "Error!"

main :: IO ()
main = do
  args <- getArgs 
  case args of 
    [fname] -> runFile fname
    []      -> runPrompt
    _       -> putStrLn $ "Usage: lox [script]"

