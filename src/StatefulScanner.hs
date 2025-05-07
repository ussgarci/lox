{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

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

data Error = Error
    { _message :: T.Text
    , _line :: Int
    }
    deriving (Show, Eq)

data Token = Token
    { _type :: TokenType
    , _lexeme :: T.Text
    , _literal :: Maybe Literal
    , _line :: Int
    }
    deriving (Show, Eq)

data ScannerState = ScannerState
    { source :: T.Text
    , start :: Int
    , current :: Int
    , line :: Int
    , tokens :: [Token]
    , errors :: [Error]
    }
    deriving (Show)

data Literal = Identifier T.Text | String T.Text | Number Double
    deriving (Show, Eq)

isAtEnd :: ScannerState -> Bool
isAtEnd ss = current ss >= T.length ss.source

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
                p <- peek
                currentState <- get
                let atEnd = isAtEnd currentState
                Control.Monad.unless
                    (p == '\n' || atEnd)
                    ( do
                        _ <- advance
                        skip
                    )
        '\r' -> scanToken
        '\t' -> scanToken
        '\n' -> do
            modify $ \s -> s{line = line s + 1}
            scanToken
        ' ' -> scanToken
        _ -> error "Unexpected character."

peek :: State ScannerState Char
peek = do
    currentState <- get
    if isAtEnd currentState
        then return '\0'
        else return $ T.index currentState.source currentState.current

advance :: State ScannerState Char
advance = do
    currentState <- get
    let c = T.index currentState.source currentState.current
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
    let currTokens = currState.tokens
    let currLine = currState.line
    modify $ \s ->
        s{tokens = currTokens ++ [Token tt (T.pack "") lit currLine]}
    return ()

match :: Char -> State ScannerState Bool
match expected = do
    currentState <- get
    if (isAtEnd currentState || (T.index currentState.source currentState.current /= expected))
        then return False
        else do
            modify $ \s ->
                let
                    nextCurrent = current s + 1
                 in
                    s{current = nextCurrent}
            return True
