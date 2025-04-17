module StatefulScanner (
    ScannerState (..),
    scanTokens,
    isAtEnd,
)
where

import Control.Monad
import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Void (Void)
import Token (Literal (..), TokenType (..))

data TokenPos = TokenPos
    { _name :: T.Text
    , _line :: !Int
    , _column :: !Int
    }
    deriving (Eq, Ord, Show)

data Token = Token
    { _type :: TokenType
    , _lexeme :: T.Text
    , _literal :: Literal
    , _position :: TokenPos
    }
    deriving (Show, Eq)

data ScannerState = ScannerState
    { source :: T.Text
    , start :: Int
    , current :: Int
    , line :: Int
    , tokens :: [Token]
    }
    deriving (Show)

isAtEnd :: ScannerState -> Bool
isAtEnd ss = current ss >= T.length (source ss)

-- runPrompt :: IO ()
-- runPrompt = go
--   where
--     go = do
--         ended <- isEOF
--         Control.Monad.unless
--             ended
--             ( do
--                 input <- getLine
--                 putStrLn input
--                 go
--             )

scanTokens :: State ScannerState ()
scanTokens = go
  where
    go = do
        currentState <- get
        let ended = isAtEnd currentState
        Control.Monad.unless
            ended
            ( do
                c <- advance
                case c of
                    '(' -> addToken LEFT_PAREN
                    _ -> undefined
                go
            )

advance :: State ScannerState Char
advance = do
    currentState <- get
    let c = T.index (source currentState) (current currentState)
    modify $ \s ->
        let
            nextLine =
                if c == '\n'
                    then line s + 1
                    else line s
            nextCurrent = current s + 1
         in
            s{current = nextCurrent, line = nextLine}
    return c

addToken :: TokenType -> State ScannerState ()
addToken = undefined

match :: Char -> State ScannerState Bool
match expected = do
    currentState <- get
    if (isAtEnd currentState || (T.index (source currentState) (current currentState) /= expected))
        then return False
        else do
            modify $ \s ->
                let
                    nextCurrent = current s + 1
                 in
                    s{current = nextCurrent}
            return True
