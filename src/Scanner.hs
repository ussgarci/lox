{-# LANGUAGE ImportQualifiedPost #-}

module Scanner (
  scanTokens
)
where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Map (Map)
import Data.Map qualified as M
import Token (TokenType(..), Token (..), Literal(..))


data Error = Error 
  { lineNumber :: Int
  , location :: String
  , message :: String
  }
  deriving (Show)

keywords :: Map String TokenType
keywords = M.fromList 
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

scanStringLiteral :: String -> String-> Int -> Int -> (Either Error Token, String, Int)
scanStringLiteral [] ys start end = (Left $ Error start ys "Unterminated string literal", [], end)
scanStringLiteral ('\"':xs) ys start end = (Right $ Token STRING ys (Just $ StringLiteral ys) start, xs, end)
scanStringLiteral ('\n':xs) ys start end = scanStringLiteral xs (ys ++ ['\n']) start (end + 1)
scanStringLiteral (x:xs) ys start end = scanStringLiteral xs (ys ++ [x]) start end


headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

scanNumberLiteral :: String -> String -> Int -> (Either Error Token, String, Int)
scanNumberLiteral [] ys ln = (Right $ Token NUMBER ys (Just $ NumberLiteral $ read ys) ln, [], ln )
scanNumberLiteral ('.':xs) ys ln = case headMaybe xs of 
  Nothing -> (Left $ Error ln (ys ++ ['.']) "Invalid number literal", xs, ln)
  Just x -> case isDigit x of
    False -> (Left $ Error ln ys "Invalid number literal", xs, ln)
    True -> (Right $ Token NUMBER lx (Just $ NumberLiteral $ read lx) ln, xs', ln)
      where 
        lx = ys ++ "." ++ takeWhile isDigit xs
        xs' = dropWhile isDigit xs
scanNumberLiteral (x:xs) ys ln = if isDigit x 
  then 
    scanNumberLiteral xs (ys ++ [x]) ln
  else
    (Right $ Token NUMBER ys (Just $ NumberLiteral $ read ys) ln, x:xs, ln)

isIdentifierChar :: Char -> Bool
isIdentifierChar x = isAlphaNum x || x == '_'

keywordCheck :: String -> Int -> Token
keywordCheck xs ln = case M.lookup xs keywords of 
  Just tt -> Token tt xs Nothing ln
  Nothing -> Token IDENTIFIER xs (Just $ IdentifierLiteral xs) ln


scanIdentifier :: String -> String -> Int -> (Either Error Token, String, Int)
scanIdentifier [] ys ln = (Right $ keywordCheck ys ln, [], ln)
scanIdentifier (x:xs) ys ln = if isIdentifierChar x 
  then 
    scanIdentifier xs (ys ++ [x]) ln
  else 
    (Right $ keywordCheck ys ln, x:xs, ln)


scanToken :: String -> Int -> (Either Error Token, String, Int)
scanToken ('\"':xs ) x = scanStringLiteral xs [] x x
scanToken ('/':'/':xs) x = scanToken (dropWhile (/= '\n') xs) x
scanToken ('\n':xs) x = scanToken xs (x+1)
scanToken ('\r':xs) x = scanToken xs (x+1)
scanToken ('\t':xs) x = scanToken xs (x+1)
scanToken (' ':xs) x = scanToken xs (x+1)
scanToken ('<':'=':xs) x = (Right $ Token LESS_EQUAL "<=" Nothing x, xs, x)
scanToken ('>':'=':xs) x = (Right $ Token GREATER_EQUAL ">=" Nothing x, xs, x)
scanToken ('=':'=':xs) x = (Right $ Token EQUAL_EQUAL "==" Nothing x, xs, x)
scanToken ('!':'=':xs) x = (Right $ Token BANG_EQUAL "!=" Nothing x, xs, x)
scanToken ('<':xs) x = (Right $ Token LESS "<" Nothing x, xs, x)
scanToken ('/':xs) x = (Right $ Token SLASH "/" Nothing x, xs, x)
scanToken ('>':xs) x = (Right $ Token GREATER ">" Nothing x, xs, x)
scanToken ('=':xs) x = (Right $ Token EQUAL "=" Nothing x, xs, x)
scanToken ('!':xs) x = (Right $ Token BANG "!" Nothing x, xs, x)
scanToken ('(':xs) x = (Right $ Token LEFT_PAREN "(" Nothing x, xs, x)
scanToken (')':xs) x = (Right $ Token RIGHT_PAREN ")" Nothing x, xs, x)
scanToken ('{':xs) x = (Right $ Token LEFT_BRACE "{" Nothing x, xs, x)
scanToken ('}':xs) x = (Right $ Token RIGHT_BRACE "}" Nothing x, xs, x)
scanToken (',':xs) x = (Right $ Token COMMA "," Nothing x, xs, x)
scanToken ('.':xs) x = (Right $ Token DOT "." Nothing x, xs, x)
scanToken ('-':xs) x = (Right $ Token MINUS "-" Nothing x, xs, x)
scanToken ('+':xs) x = (Right $ Token PLUS "+" Nothing x, xs, x)
scanToken (';':xs) x = (Right $ Token SEMICOLON ";" Nothing x, xs, x)
scanToken ('*':xs) x = (Right $ Token STAR "*" Nothing x, xs, x)
scanToken (x:xs) y  
  | isDigit x = scanNumberLiteral (x:xs) [] y 
  | isAlpha x = scanIdentifier (x:xs) [] y
  | otherwise = (Left $ Error y [x] "unknown char in scanner", xs, y)  
scanToken [] x = (Right $ Token EOF "" Nothing x, [], x)  



