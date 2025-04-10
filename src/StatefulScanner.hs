module StatefulScanner (
    scan
)
where

import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.Map as M
import Token (Literal (..), Token (..), TokenType (..), TokenPos (..))

type Line = Int
type Column = Int
type ScannerState = (Line, Column)

startState :: ScannerState
startState = (0,0)

scan :: String -> State ScannerState [Token]
scan = undefined
-- scan [] = do
--     (ln, cl) <- get
--     return Right $ Token EOF "" Nothing TokenPos "" ln cl
-- scan (x:xs) = undefined