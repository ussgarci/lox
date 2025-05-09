{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module StatefulScanner (
    ScannerState (..),
    scanTokens,
    isAtEnd,
)
where

import Control.Monad (unless, when)
import Control.Monad.Extra (ifM, notM)
import Control.Monad.Loops (whileM_)
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
    (c, ln) <- advance
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
        _ -> do
            _ <- logError (T.pack "Unexpected character") ln
            scanToken

-- private void string() {
--   while (peek() != '"' && !isAtEnd()) {
--     if (peek() == '\n') line++;
--     advance();
--   }

--   if (isAtEnd()) {
--     Lox.error(line, "Unterminated string.");
--     return;
--   }

--   // The closing ".
--   advance();

--   // Trim the surrounding quotes.
--   String value = source.substring(start + 1, current - 1);
--   addToken(STRING, value);
-- }

string :: State ScannerState ()
string = do
    whileM_
        ( do
            c <- peek
            currentState <- get
            return $ c /= '"' && isAtEnd currentState
        )
        ( do
            advance
        )
    when gets isAtEnd $ error "AAAAAH"
    advance
    let val = undefined -- TBD
    addToken STRING (Just $ String val)
    return ()

logError :: T.Text -> Int -> State ScannerState ()
logError msg ln = do
    modify $ \s ->
        s{errors = s.errors ++ [Error msg ln]}

peek :: State ScannerState Char
peek = do
    currentState <- get
    if isAtEnd currentState
        then return '\0'
        else return $ T.index currentState.source currentState.current

advance :: State ScannerState (Char, Int)
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
    return (c, currentState.line)

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
