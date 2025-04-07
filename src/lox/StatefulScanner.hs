module StatefulScanner (
    scan
)
where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Map (Map)
import Data.Map qualified as M
import Token (Literal (..), Token (..), TokenType (..), TokenPos (..))

type Line = Int
type Column = Int
type ScannerState = (Line, Column)

startState = (0,0)

scan :: String -> State ScannerState [Either Error Token]
scan [] = do
    (ln, cl) <- get
    return Right $ Token EOF "" Nothing TokenPos "" ln cl
scan (x:xs) = undefined

