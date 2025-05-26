{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module StatefulScanner (
    ScannerState (..),
    Token (..),
    scanTokens,
    scanToken,
    isAtEnd,
)
where

import Control.Monad (unless, when)
import Control.Monad.Extra (ifM, notM, whenM)
import Control.Monad.Loops (whileM_)
import Control.Monad.State
import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Read (readMaybe)
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

keywords :: M.Map T.Text TokenType
keywords =
    M.fromList $
        map
            (first T.pack)
            [ ("and", AND)
            , ("class", CLASS)
            , ("else", ELSE)
            , ("false", FALSE)
            , ("for", FOR)
            , ("fun", FUN)
            , ("if", IF)
            , ("nil", NIL)
            , ("or", OR)
            , ("print", PRINT)
            , ("return", RETURN)
            , ("super", SUPER)
            , ("this", THIS)
            , ("true", TRUE)
            , ("var", VAR)
            , ("while", WHILE)
            ]

isAtEnd :: ScannerState -> Bool
isAtEnd ss = current ss >= T.length ss.source

scanTokens :: State ScannerState ()
scanTokens = go
  where
    go = do
        currentState <- get
        let ended = isAtEnd currentState
        modify $ \s -> s{start = s.current}
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
        '\r' -> return ()
        '\t' -> return ()
        '\n' -> return ()
        ' ' -> return ()
        '"' -> string
        _ -> do
            if isDigit c
                then number
                else
                    if isAlpha c
                        then identifier
                        else do
                            _ <- logError (T.pack "Unexpected character") ln
                            scanToken

--  private void identifier() {
--    while (isAlphaNumeric(peek())) advance();
--
--    addToken(IDENTIFIER);
--  }

identifier :: State ScannerState ()
identifier = do
    whileM_
        ( do
            isAlphaNum <$> peek
        )
        ( do
            advance
        )
    finalState <- get
    let text = slice finalState.start finalState.current finalState.source
    case M.lookup text keywords of
        Just tokenType -> addToken tokenType (Just $ Identifier text)
        Nothing -> addToken IDENTIFIER (Just $ Identifier text)

--  private void number() {
--    while (isDigit(peek())) advance();
--
--    // Look for a fractional part.
--    if (peek() == '.' && isDigit(peekNext())) {
--      // Consume the "."
--      advance();
--
--      while (isDigit(peek())) advance();
--    }
--
--    addToken(NUMBER,
--        Double.parseDouble(source.substring(start, current)));
--  }

slice :: Int -> Int -> T.Text -> T.Text
slice a b = T.take (b - a) . T.drop a

number :: State ScannerState ()
number = do
    whileM_
        ( do
            -- c <- peek
            -- return (isDigit c)
            isDigit <$> peek
        )
        ( do
            advance
        )
    hasFractionalPart <- liftM2 (&&) ((== '.') <$> peek) (isDigit <$> peekNext)

    when
        hasFractionalPart
        ( do
            _ <- advance
            whileM_
                ( do
                    isDigit <$> peek
                )
                ( do
                    advance
                )
        )
    finalState <- get
    let textVal = slice finalState.start finalState.current finalState.source
    case readMaybe (T.unpack textVal) :: Maybe Double of
        Just val -> addToken NUMBER (Just $ Number val)
        Nothing -> logError (T.pack $ "Failed to convert " ++ T.unpack textVal ++ " to Double.") finalState.line

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
            currentState <- get
            if isAtEnd currentState
                then return False -- Stop if at the end
                else do
                    c <- peek
                    return (c /= '"') -- Continue if not a quote
        )
        ( do
            advance
        )
    currentState <- get
    -- loop terminated with EOF
    if isAtEnd currentState
        then do
            modify $ \s ->
                s{errors = s.errors ++ [Error (T.pack "Unterminated string literal") s.line]}
        else -- loop terminated with closing double quote
        do
            -- Consume closing double quote
            _ <- advance
            finalState <- get
            let val = slice (finalState.start + 1) (finalState.current - 1) finalState.source
            addToken STRING (Just $ String val)

logError :: T.Text -> Int -> State ScannerState ()
logError msg ln = do
    modify $ \s ->
        s{errors = s.errors ++ [Error msg ln]}

peekNext :: State ScannerState Char
peekNext = do
    currentState <- get
    if (currentState.current + 1) == T.length currentState.source
        then return '\0'
        else return $ T.index currentState.source (currentState.current + 1)

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
    let text = slice currState.start currState.current currState.source
    modify $ \s ->
        s{tokens = currTokens ++ [Token tt text lit currLine]}
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
