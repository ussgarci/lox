{-# LANGUAGE LambdaCase #-}

module StatefulScanner (
    ScannerState (..),
    scanTokens,
    isAtEnd,
)
where

import Control.Monad
import Control.Monad.Extra (ifM, notM, whileM)
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
        '!' ->
            ifM
                (match '=')
                (addToken BANG_EQUAL Nothing)
                (addToken BANG Nothing)
        '=' ->
            ifM
                (match '=')
                (addToken EQUAL_EQUAL Nothing)
                (addToken EQUAL Nothing)
        '<' ->
            ifM
                (match '=')
                (addToken LESS_EQUAL Nothing)
                (addToken LESS Nothing)
        '>' ->
            ifM
                (match '=')
                (addToken GREATER_EQUAL Nothing)
                (addToken GREATER Nothing)
        '/' ->
            ifM
                (notM $ match '/')
                (addToken SLASH Nothing)
                skip
          where
            skip = do
                c <- peek
                let atEnd = get () >= isAtEnd
                Control.Monad.unless
                    c
                    == '\n'
                    || atEnd
                        ( do
                            advance
                            skip
                        )
                return
        '\r' -> scanToken
        '\t' -> scanToken
        '\n' -> do
            modify $ \s -> s{line = line s + 1}
        ' ' -> scanToken
        _ -> error "Unexpected character."

peek :: State ScannerState Char
peek = do
    currentState <- get
    return T.index (source currentState) (current currentState)

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
