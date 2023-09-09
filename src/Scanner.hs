
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

data ScannerState = ScannerState 
  { currentLine :: Int
  }

scanTokens :: String -> Maybe [Token]
scanTokens xs = go (lines xs) 0
  where
    go :: [String] -> Int -> Maybe [Token]
    go [] _ = Just []
    go (currLine:xss) x = case scanLine currLine x of 
      Nothing     -> Nothing
      tokens -> tokens <> (go xss (x+1))

scanLine :: String -> Int -> Maybe [Token]
scanLine [] _ = Just []
scanLine xs x = case scanToken xs x of 
  Just (token, xs') -> fmap ((:) token) (scanLine xs' x)
  Nothing           -> Nothing

scanToken :: String -> Int -> Maybe (Token, String)
scanToken ('<':'=':xs) x = Just $ (Token LESS_EQUAL "<=" x, xs)
scanToken ('>':'=':xs) x = Just $ (Token GREATER_EQUAL ">=" x, xs)
scanToken ('=':'=':xs) x = Just $ (Token EQUAL_EQUAL "==" x, xs)
scanToken ('!':'=':xs) x = Just $ (Token BANG_EQUAL "!=" x, xs)
scanToken ('<':xs) x = Just $ (Token LESS "<" x, xs)
scanToken ('>':xs) x = Just $ (Token GREATER ">" x, xs)
scanToken ('=':xs) x = Just $ (Token EQUAL "=" x, xs)
scanToken ('!':xs) x = Just $ (Token BANG "!" x, xs)
scanToken ('(':xs) x = Just $ (Token LEFT_PAREN "(" x, xs)
scanToken (')':xs) x = Just $ (Token RIGHT_PAREN ")" x, xs)
scanToken ('{':xs) x = Just $ (Token LEFT_BRACE "{" x, xs)
scanToken ('}':xs) x = Just $ (Token RIGHT_BRACE "}" x, xs)
scanToken (',':xs) x = Just $ (Token COMMA "," x, xs)
scanToken ('.':xs) x = Just $ (Token DOT "." x, xs)
scanToken ('-':xs) x = Just $ (Token MINUS "-" x, xs)
scanToken ('+':xs) x = Just $ (Token PLUS "+" x, xs)
scanToken (';':xs) x = Just $ (Token SEMICOLON ";" x, xs)
scanToken ('*':xs) x = Just $ (Token STAR "*" x, xs)
scanToken _ _ = Nothing



