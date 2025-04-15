module StatefulScanner (
    ScannerState (..),
    scanTokens,
    isAtEnd,
)
where

import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.Map as M
import qualified Data.Text as T
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
    }
    deriving (Show)

isAtEnd :: ScannerState -> Bool
isAtEnd ss = current ss >= T.length (source ss)

scanTokens :: State ScannerState [Token]
scanTokens = undefined

-- scan [] = do
--     (ln, cl) <- get
--     return Right $ Token EOF "" Nothing TokenPos "" ln cl
-- scan (x:xs) = undefined

scanToken :: State ScannerState [Token]
scanToken = do
    c <- advance
    undefined

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
