
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
scanTokens xs = scanLines (lines xs) 0

scanLines :: [String] -> Int -> [Either Error Token]
scanLines [] _ = []
scanLines (xs:xss) lineNum = (scanLine xs lineNum) <> (scanLines xss (lineNum +1))


scanLine :: String -> Int -> [Either Error Token]
scanLine [] _ = []
scanLine ('/':'/':_) _ = []
scanLine (' ':xs) x = scanLine xs x
scanLine xs x = token : scanLine xs' x 
  where (token, xs') = scanToken xs x 

scanToken :: String -> Int -> (Either Error Token, String)
scanToken ('<':'=':xs) x = (Right $ Token LESS_EQUAL "<=" x, xs)
scanToken ('>':'=':xs) x = (Right $ Token GREATER_EQUAL ">=" x, xs)
scanToken ('=':'=':xs) x = (Right $ Token EQUAL_EQUAL "==" x, xs)
scanToken ('!':'=':xs) x = (Right $ Token BANG_EQUAL "!=" x, xs)
scanToken ('<':xs) x = (Right $ Token LESS "<" x, xs)
scanToken ('/':xs) x = (Right $ Token SLASH "/" x, xs)
scanToken ('>':xs) x = (Right $ Token GREATER ">" x, xs)
scanToken ('=':xs) x = (Right $ Token EQUAL "=" x, xs)
scanToken ('!':xs) x = (Right $ Token BANG "!" x, xs)
scanToken ('(':xs) x = (Right $ Token LEFT_PAREN "(" x, xs)
scanToken (')':xs) x = (Right $ Token RIGHT_PAREN ")" x, xs)
scanToken ('{':xs) x = (Right $ Token LEFT_BRACE "{" x, xs)
scanToken ('}':xs) x = (Right $ Token RIGHT_BRACE "}" x, xs)
scanToken (',':xs) x = (Right $ Token COMMA "," x, xs)
scanToken ('.':xs) x = (Right $ Token DOT "." x, xs)
scanToken ('-':xs) x = (Right $ Token MINUS "-" x, xs)
scanToken ('+':xs) x = (Right $ Token PLUS "+" x, xs)
scanToken (';':xs) x = (Right $ Token SEMICOLON ";" x, xs)
scanToken ('*':xs) x = (Right $ Token STAR "*" x, xs)
scanToken (y:xs) x = (Left $ Error x [y] "unknown char in scanner", xs)  
scanToken [] x = (Left $ Error x "" "scanToken received empty string", [])  



