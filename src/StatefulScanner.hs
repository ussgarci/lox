module StatefulScanner (
    ScannerState (..),
    scanTokens,
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

-- startState :: ScannerState
-- startState = ScannerState 0 0 1

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
    state <- get
    let curr = current state + 1
    let src = source state
    return $ T.index src curr
