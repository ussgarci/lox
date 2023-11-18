
module Token (
    Token (..),
    TokenType (..),
    Literal (..)
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
  deriving (Show, Eq)

data Literal =IdentifierLiteral String | StringLiteral String | NumberLiteral Double 
  deriving (Show, Eq)

data Token = Token 
  { _type :: TokenType
  , _lexeme :: String 
  , _literal :: Maybe Literal
  , _line :: Int 
  }
  deriving (Show, Eq)

