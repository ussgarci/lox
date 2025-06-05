{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parser (
    parse,
)
where

import Control.Monad.State
import StatefulScanner (Token)

data ParserState = ParserState
    { current :: Int
    , tokens :: [Token]
    }
    deriving (Show)

data Expr where
    Binary :: Expr -> Token -> Expr -> Expr
    Unary :: Token -> Expr -> Expr
    Literal :: Token -> Expr
    Grouping :: Expr -> Expr

deriving instance Show Expr

-- expression     → equality ;
-- equality       → comparison ( ( "!=" | "==" ) comparison )* ;
-- comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
-- term           → factor ( ( "-" | "+" ) factor )* ;
-- factor         → unary ( ( "/" | "*" ) unary )* ;
-- unary          → ( "!" | "-" ) unary
--                | primary ;
-- primary        → NUMBER | STRING | "true" | "false" | "nil"
--                | "(" expression ")" ;

type Parser a = State ParserState a

parse :: Parser Expr
parse = do
    expr <- expression
    undefined

expression :: Parser Expr
expression = equality

equality :: Parser Expr
equality = undefined
