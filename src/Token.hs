module Token (
    Token (..),
    TokenType (..),
    TokenPos (..),
    Literal (..),
)
where

data TokenType
    = -- Single-character tokens.
      LEFT_PAREN
    | RIGHT_PAREN
    | LEFT_BRACE
    | RIGHT_BRACE
    | COMMA
    | DOT
    | MINUS
    | PLUS
    | SEMICOLON
    | SLASH
    | STAR
    | -- One or two character tokens.
      BANG
    | BANG_EQUAL
    | EQUAL
    | EQUAL_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL
    | -- Literals.
      IDENTIFIER
    | STRING
    | NUMBER
    | -- Keywords.
      AND
    | CLASS
    | ELSE
    | FALSE
    | FUN
    | FOR
    | IF
    | NIL
    | OR
    | PRINT
    | RETURN
    | SUPER
    | THIS
    | TRUE
    | VAR
    | WHILE
    | EOF
    deriving (Show, Eq)

data Literal = IdentifierLiteral String | StringLiteral String | NumberLiteral Double
    deriving (Show, Eq)

-- MegaParsec SourcePos
-- data SourcePos = SourcePos
--   { -- | Extract the name of the source from a source position.
--     sourceName   :: String
--     -- | Extract the line number from a source position.
--   , sourceLine   :: !Int
--     -- | Extract the column number from a source position.
--   , sourceColumn :: !Int }
--   deriving (Eq, Ord, Data, Typeable)

data TokenPos = TokenPos
    { _name :: String
    , _line :: !Int
    , _column :: !Int
    }
    -- deriving (Eq, Ord, Data, Typeable)
    -- What are Data and Typeable?
    -- import Data.Data (Data)
    -- import Data.Typeable (Typeable)
    deriving (Eq, Ord, Show)

data Token = Token
    { _type :: TokenType
    , _lexeme :: String
    , _literal :: Maybe Literal
    , _position :: TokenPos
    }
    deriving (Show, Eq)
