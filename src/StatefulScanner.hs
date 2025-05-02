{-# LANGUAGE LambdaCase #-}

module StatefulScanner (
    ScannerState (..),
    scanTokens,
    isAtEnd,
)
where

import Control.Monad
import Control.Monad.State
import qualified Data.Text as T
import Token (TokenType (..))

-- data TokenPos = TokenPos
--     { _name :: T.Text
--     , _line :: !Int
--     , _column :: !Int
--     }
--     deriving (Eq, Ord, Show)

data Token = Token
    { _type :: TokenType
    , _lexeme :: T.Text
    , _literal :: Maybe Literal
    , _line :: Int
    -- , _position :: TokenPos
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

data Literal = Identifier T.Text | String T.Text | Number Double
    deriving (Show, Eq)

isAtEnd :: ScannerState -> Bool
isAtEnd ss = current ss >= T.length (source ss)

scanTokens :: State ScannerState ()
scanTokens = go
  where
    go = do
        currentState <- get
        let ended = isAtEnd currentState
        Control.Monad.unless
            ended
            ( do
                scanToken
                go
            )

scanToken :: State ScannerState ()
scanToken = do
    c <- advance
    case c of
        '(' -> addToken LEFT_PAREN Nothing
        ')' -> addToken RIGHT_PAREN Nothing
        '{' -> addToken LEFT_BRACE Nothing
        '}' -> addToken RIGHT_BRACE Nothing
        ',' -> addToken COMMA Nothing
        '.' -> addToken DOT Nothing
        '-' -> addToken MINUS Nothing
        '+' -> addToken PLUS Nothing
        ';' -> addToken SEMICOLON Nothing
        '*' -> addToken STAR Nothing
        ' ' -> scanToken
        _ -> error "Unexpected character."

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

addToken :: TokenType -> Maybe Literal -> State ScannerState ()
--     tokens.add(new Token(type, text, literal, line));
addToken tt lit = do
    currState <- get
    let currTokens = tokens currState
    let currLine = line currState
    modify $ \s ->
        s{tokens = currTokens ++ [Token tt (T.pack "") lit currLine]}
    return ()

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
