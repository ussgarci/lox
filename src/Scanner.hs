{-# LANGUAGE ImportQualifiedPost #-}

module Scanner (
  scanTokens
)
where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Map (Map)
import Data.Map qualified as M

data TokenType = 
  -- Single-character tokens.
  LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE |
  COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH | STAR |

  -- One or two character tokens.
  BANG | BANG_EQUAL | EQUAL | EQUAL_EQUAL |
  GREATER | GREATER_EQUAL | LESS | LESS_EQUAL |

  -- Literals. 
  IDENTIFIER | STRING | NUMBER |

  -- Keywords.
  AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR |
  PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE | EOF
  deriving (Show, Eq)

data Literal =IdentifierLiteral String | StringLiteral String | NumberLiteral Double | Nil
  deriving (Show, Eq)

-- How to mitigate record accessor clashes?
data Token = Token 
  { _type :: TokenType
  , lexeme :: String 
  , literal :: Literal
  , line :: Int 
  }
  deriving (Show, Eq)

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
scanStringLiteral ('\"':xs) ys start end = (Right $ Token STRING ys (StringLiteral ys) start, xs, end)
scanStringLiteral ('\n':xs) ys start end = scanStringLiteral xs (ys ++ ['\n']) start (end + 1)
scanStringLiteral (x:xs) ys start end = scanStringLiteral xs (ys ++ [x]) start end


headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

scanNumberLiteral :: String -> String -> Int -> (Either Error Token, String, Int)
scanNumberLiteral [] ys ln = (Right $ Token NUMBER ys (NumberLiteral $ read ys) ln, [], ln )
scanNumberLiteral ('.':xs) ys ln = case headMaybe xs of 
  Nothing -> (Left $ Error ln (ys ++ ['.']) "Invalid number literal", xs, ln)
  Just x -> case isDigit x of
    False -> (Left $ Error ln ys "Invalid number literal", xs, ln)
    True -> (Right $ Token NUMBER lx (NumberLiteral $ read lx) ln, xs', ln)
      where 
        lx = ys ++ "." ++ takeWhile isDigit xs
        xs' = dropWhile isDigit xs
scanNumberLiteral (x:xs) ys ln = if isDigit x 
  then 
    scanNumberLiteral xs (ys ++ [x]) ln
  else
    (Right $ Token NUMBER ys (NumberLiteral $ read ys) ln, x:xs, ln)

isIdentifierChar :: Char -> Bool
isIdentifierChar x = isAlphaNum x || x == '_'

keywordCheck :: String -> Int -> Token
keywordCheck xs ln = case M.lookup xs keywords of 
  Just tt -> Token tt xs Nil ln
  Nothing -> Token IDENTIFIER xs (IdentifierLiteral xs) ln


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
scanToken ('<':'=':xs) x = (Right $ Token LESS_EQUAL "<=" Nil x, xs, x)
scanToken ('>':'=':xs) x = (Right $ Token GREATER_EQUAL ">=" Nil x, xs, x)
scanToken ('=':'=':xs) x = (Right $ Token EQUAL_EQUAL "==" Nil x, xs, x)
scanToken ('!':'=':xs) x = (Right $ Token BANG_EQUAL "!=" Nil x, xs, x)
scanToken ('<':xs) x = (Right $ Token LESS "<" Nil x, xs, x)
scanToken ('/':xs) x = (Right $ Token SLASH "/" Nil x, xs, x)
scanToken ('>':xs) x = (Right $ Token GREATER ">" Nil x, xs, x)
scanToken ('=':xs) x = (Right $ Token EQUAL "=" Nil x, xs, x)
scanToken ('!':xs) x = (Right $ Token BANG "!" Nil x, xs, x)
scanToken ('(':xs) x = (Right $ Token LEFT_PAREN "(" Nil x, xs, x)
scanToken (')':xs) x = (Right $ Token RIGHT_PAREN ")" Nil x, xs, x)
scanToken ('{':xs) x = (Right $ Token LEFT_BRACE "{" Nil x, xs, x)
scanToken ('}':xs) x = (Right $ Token RIGHT_BRACE "}" Nil x, xs, x)
scanToken (',':xs) x = (Right $ Token COMMA "," Nil x, xs, x)
scanToken ('.':xs) x = (Right $ Token DOT "." Nil x, xs, x)
scanToken ('-':xs) x = (Right $ Token MINUS "-" Nil x, xs, x)
scanToken ('+':xs) x = (Right $ Token PLUS "+" Nil x, xs, x)
scanToken (';':xs) x = (Right $ Token SEMICOLON ";" Nil x, xs, x)
scanToken ('*':xs) x = (Right $ Token STAR "*" Nil x, xs, x)
scanToken (x:xs) y  
  | isDigit x = scanNumberLiteral (x:xs) [] y 
  | isAlpha x = scanIdentifier (x:xs) [] y
  | otherwise = (Left $ Error y [x] "unknown char in scanner", xs, y)  
scanToken [] x = (Right $ Token EOF "" Nil x, [], x)  



