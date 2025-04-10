module StatefulScanner (
    scanTokens
)
where

import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.Map as M
import Token (Literal (..), Token (..), TokenType (..), TokenPos (..))

data ScannerState = ScannerState
    { source :: String
    , start :: Int
    , current :: Int
    , line :: Int
    }
    deriving (Show)

--startState :: ScannerState
--startState = ScannerState 0 0 1 

isAtEnd :: ScannerState -> Bool
isAtEnd ss = current ss >= length (source ss)

scanTokens :: String -> State ScannerState [Token]
scanTokens = undefined
-- scan [] = do
--     (ln, cl) <- get
--     return Right $ Token EOF "" Nothing TokenPos "" ln cl
-- scan (x:xs) = undefined

scanToken :: State ScannerState [Token]
scanToken = do
    c <- advance
    undefined

advance:: State ScannerState Char
advance = do
   state <- get
   let curr = current state + 1
   let src = source state
   return (src !! curr)