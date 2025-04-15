{-# LANGUAGE ImportQualifiedPost #-}

module Scanner (
    scanTokens,
)
where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Map (Map)
import Data.Map qualified as M
import Token (Literal (..), Token (..), TokenPos (..), TokenType (..))

ln2pos :: Int -> TokenPos
ln2pos ln =
    TokenPos
        { _name = ""
        , _line = ln
        , _column = 0
        }

data Error = Error
    { lineNumber :: Int
    , location :: String
    , message :: String
    }
    deriving (Show)

keywords :: Map String TokenType
keywords =
    M.fromList
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

scanTokens :: String -> [Either Error Token]
scanTokens source = go source 0
  where
    go [] _ = []
    go xs x = result : go xs' x'
      where
        (result, xs', x') = scanToken xs x

scanStringLiteral :: String -> String -> Int -> Int -> (Either Error Token, String, Int)
scanStringLiteral [] ys start end = (Left $ Error start ys "Unterminated string literal", [], end)
scanStringLiteral ('\"' : xs) ys start end = (Right $ Token STRING ys (Just $ StringLiteral ys) (ln2pos start), xs, end)
scanStringLiteral ('\n' : xs) ys start end = scanStringLiteral xs (ys ++ ['\n']) start (end + 1)
scanStringLiteral (x : xs) ys start end = scanStringLiteral xs (ys ++ [x]) start end

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

scanNumberLiteral :: String -> String -> Int -> (Either Error Token, String, Int)
scanNumberLiteral [] ys ln = (Right $ Token NUMBER ys (Just $ NumberLiteral $ read ys) (ln2pos ln), [], ln)
scanNumberLiteral ('.' : xs) ys ln = case headMaybe xs of
    Nothing -> (Left $ Error ln (ys ++ ['.']) "Invalid number literal", xs, ln)
    Just x -> case isDigit x of
        False -> (Left $ Error ln ys "Invalid number literal", xs, ln)
        True -> (Right $ Token NUMBER lx (Just $ NumberLiteral $ read lx) (ln2pos ln), xs', ln)
          where
            lx = ys ++ "." ++ takeWhile isDigit xs
            xs' = dropWhile isDigit xs
scanNumberLiteral (x : xs) ys ln =
    if isDigit x
        then scanNumberLiteral xs (ys ++ [x]) ln
        else (Right $ Token NUMBER ys (Just $ NumberLiteral $ read ys) (ln2pos ln), x : xs, ln)

isIdentifierChar :: Char -> Bool
isIdentifierChar x = isAlphaNum x || x == '_'

keywordCheck :: String -> Int -> Token
keywordCheck xs ln = case M.lookup xs keywords of
    Just tt -> Token tt xs Nothing (ln2pos ln)
    Nothing -> Token IDENTIFIER xs (Just $ IdentifierLiteral xs) (ln2pos ln)

scanIdentifier :: String -> String -> Int -> (Either Error Token, String, Int)
scanIdentifier [] ys ln = (Right $ keywordCheck ys ln, [], ln)
scanIdentifier (x : xs) ys ln =
    if isIdentifierChar x
        then scanIdentifier xs (ys ++ [x]) ln
        else (Right $ keywordCheck ys ln, x : xs, ln)

scanToken :: String -> Int -> (Either Error Token, String, Int)
scanToken xs ln = case xs of
    ('\"' : rest) -> scanStringLiteral rest [] ln ln
    ('/' : '/' : rest) -> scanToken (dropWhile (/= '\n') rest) ln
    ('\n' : rest) -> scanToken rest (ln + 1)
    ('\r' : rest) -> scanToken rest (ln + 1)
    ('\t' : rest) -> scanToken rest (ln + 1)
    (' ' : rest) -> scanToken rest (ln + 1)
    ('<' : '=' : rest) -> (Right $ Token LESS_EQUAL "<=" Nothing pos, rest, ln)
    ('>' : '=' : rest) -> (Right $ Token GREATER_EQUAL ">=" Nothing pos, rest, ln)
    ('=' : '=' : rest) -> (Right $ Token EQUAL_EQUAL "==" Nothing pos, rest, ln)
    ('!' : '=' : rest) -> (Right $ Token BANG_EQUAL "!=" Nothing pos, rest, ln)
    ('<' : rest) -> (Right $ Token LESS "<" Nothing pos, rest, ln)
    ('/' : rest) -> (Right $ Token SLASH "/" Nothing pos, rest, ln)
    ('>' : rest) -> (Right $ Token GREATER ">" Nothing pos, rest, ln)
    ('=' : rest) -> (Right $ Token EQUAL "=" Nothing pos, rest, ln)
    ('!' : rest) -> (Right $ Token BANG "!" Nothing pos, rest, ln)
    ('(' : rest) -> (Right $ Token LEFT_PAREN "(" Nothing pos, rest, ln)
    (')' : rest) -> (Right $ Token RIGHT_PAREN ")" Nothing pos, rest, ln)
    ('{' : rest) -> (Right $ Token LEFT_BRACE "{" Nothing pos, rest, ln)
    ('}' : rest) -> (Right $ Token RIGHT_BRACE "}" Nothing pos, rest, ln)
    (',' : rest) -> (Right $ Token COMMA "," Nothing pos, rest, ln)
    ('.' : rest) -> (Right $ Token DOT "." Nothing pos, rest, ln)
    ('-' : rest) -> (Right $ Token MINUS "-" Nothing pos, rest, ln)
    ('+' : rest) -> (Right $ Token PLUS "+" Nothing pos, rest, ln)
    (';' : rest) -> (Right $ Token SEMICOLON ";" Nothing pos, rest, ln)
    ('*' : rest) -> (Right $ Token STAR "*" Nothing pos, rest, ln)
    (x : rest)
        | isDigit x -> scanNumberLiteral (x : rest) [] ln
        | isAlpha x -> scanIdentifier (x : rest) [] ln
        | otherwise -> (Left $ Error ln [x] "unknown char in scanner", rest, ln)
    [] -> (Right $ Token EOF "" Nothing pos, [], ln)
  where
    pos = ln2pos ln
