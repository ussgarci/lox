{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parser (
    parse,
)
where

import Control.Monad.Loops (whileM_)
import Control.Monad.State
import StatefulScanner (Token)
import Token (TokenType (..))

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

-- equality       → comparison ( ( "!=" | "==" ) comparison )* ;
-- private Expr equality() {
--   Expr expr = comparison();

--   while (match(BANG_EQUAL, EQUAL_EQUAL)) {
--     Token operator = previous();
--     Expr right = comparison();
--     expr = new Expr.Binary(expr, operator, right);
--   }

--   return expr;
-- }
equality :: Parser Expr
equality = do
    let expr = comparison
    whileM_
        ( do
            match BANG_EQUAL EQUAL_EQUAL
        )
        ( do
            operator <- previous
            undefined
        )
    undefined

comparison :: Parser Expr
comparison = undefined

match :: TokenType -> TokenType -> Parser Bool
match = undefined

-- return tokens.get(current - 1);
previous :: Parser Token
previous = do
    currentState <- get
    let prev = currentState.tokens !! (currentState.current - 1)
    return prev
