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

parse :: State ParserState Expr
parse = undefined

expression :: State ParserState Expr
expression = undefined

equality :: State ParserState Expr
equality = undefined
