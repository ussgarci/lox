
module Scanner (
  scanTokens
)
where

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
  deriving (Show)

-- How to mitigate record accessor clashes?
data Token = Token 
  { _type :: TokenType
  , lexeme :: String 
  , line :: Int 
  }
  deriving (Show)

data Error = Error 
  { lineNumber :: Int
  , location :: String
  , message :: String
  }
  deriving (Show)

-- data ScannerState = ScannerState 
--   { currentLine :: Int
--   }

scanTokens :: String -> [Either Error Token]
scanTokens source = go source 0
  where
    go [] _ = []
    go xs x = result : go xs' x'
      where
        (result, xs', x') = scanToken xs x

scanStringLiteral :: String -> String-> Int -> Int -> (Either Error Token, String, Int)
scanStringLiteral [] ys start end = (Left $ Error start ys "Unterminated string literal", [], end)
scanStringLiteral ('\"':xs) ys start end = (Right $ Token STRING ys start, xs, end)
scanStringLiteral ('\n':xs) ys start end = scanStringLiteral xs (ys ++ ['\n']) start (end + 1)
scanStringLiteral (x:xs) ys start end = scanStringLiteral xs (ys ++ [x]) start end


scanToken :: String -> Int -> (Either Error Token, String, Int)
-- "number literals"
scanToken ('\"':xs ) x = scanStringLiteral xs [] x x
scanToken ('/':'/':xs) x = scanToken (dropWhile (\y -> y /= '\n') xs) x
scanToken ('\n':xs) x = scanToken xs (x+1)
scanToken ('\r':xs) x = scanToken xs (x+1)
scanToken ('\t':xs) x = scanToken xs (x+1)
scanToken (' ':xs) x = scanToken xs (x+1)
scanToken ('<':'=':xs) x = (Right $ Token LESS_EQUAL "<=" x, xs, x)
scanToken ('>':'=':xs) x = (Right $ Token GREATER_EQUAL ">=" x, xs, x)
scanToken ('=':'=':xs) x = (Right $ Token EQUAL_EQUAL "==" x, xs, x)
scanToken ('!':'=':xs) x = (Right $ Token BANG_EQUAL "!=" x, xs, x)
scanToken ('<':xs) x = (Right $ Token LESS "<" x, xs, x)
scanToken ('/':xs) x = (Right $ Token SLASH "/" x, xs, x)
scanToken ('>':xs) x = (Right $ Token GREATER ">" x, xs, x)
scanToken ('=':xs) x = (Right $ Token EQUAL "=" x, xs, x)
scanToken ('!':xs) x = (Right $ Token BANG "!" x, xs, x)
scanToken ('(':xs) x = (Right $ Token LEFT_PAREN "(" x, xs, x)
scanToken (')':xs) x = (Right $ Token RIGHT_PAREN ")" x, xs, x)
scanToken ('{':xs) x = (Right $ Token LEFT_BRACE "{" x, xs, x)
scanToken ('}':xs) x = (Right $ Token RIGHT_BRACE "}" x, xs, x)
scanToken (',':xs) x = (Right $ Token COMMA "," x, xs, x)
scanToken ('.':xs) x = (Right $ Token DOT "." x, xs, x)
scanToken ('-':xs) x = (Right $ Token MINUS "-" x, xs, x)
scanToken ('+':xs) x = (Right $ Token PLUS "+" x, xs, x)
scanToken (';':xs) x = (Right $ Token SEMICOLON ";" x, xs, x)
scanToken ('*':xs) x = (Right $ Token STAR "*" x, xs, x)
scanToken (y:xs) x = (Left $ Error x [y] "unknown char in scanner", xs, x)  
scanToken [] x = (Right $ Token EOF "" x, [], x)  



