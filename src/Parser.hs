{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parser (
    parse,
)
where

import Control.Monad.Extra (ifM, notM, whenM)
import Control.Monad.Loops (whileM_)
import Control.Monad.State
import StatefulScanner (Token (..))
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
    expression

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
    expr <- comparison
    whileM_
        ( do
            match [BANG_EQUAL, EQUAL_EQUAL]
        )
        ( do
            operator <- previous
            right <- comparison
            -- expr = new Expr.Binary(expr, operator, right);
            undefined
        )
    return expr

comparison :: Parser Expr
comparison = do
    expr <- term
    whileM_
        ( do
            match [MINUS, PLUS]
        )
        ( do
            operator <- previous
            right <- factor
            -- expr = new Expr.Binary(expr, operator, right);
            undefined
        )
    return expr

factor :: Parser Expr
factor = do
    expr <- unary
    whileM_
        ( do
            match [SLASH, STAR]
        )
        ( do
            operator <- previous
            right <- unary
            -- expr = new Expr.Binary(expr, operator, right);
            undefined
        )
    return expr

unary :: Parser Expr
unary = do
    ifM
        (match [BANG, MINUS])
        ( do
            operator <- previous
            right <- unary
            -- expr = new Expr.Unary(operator, right);
            undefined
        )
        ( do
            primary
        )

primary :: Parser Expr
primary = undefined

term :: Parser Expr
term = undefined

--  private boolean check(TokenType type) {
--    if (isAtEnd()) return false;
--    return peek().type == type;
--  }
check :: TokenType -> Parser Bool
check = do
    undefined

--  private boolean match(TokenType... types) {
--    for (TokenType type : types) {
--      if (check(type)) {
--        advance();
--        return true;
--      }
--    }
--
--    return false;
--  }
match :: [TokenType] -> Parser Bool
match = undefined

--  private boolean isAtEnd() {
--    return peek().type == EOF;
--  }
isAtEnd :: Parser Bool
isAtEnd = do
    token <- peek
    return $ token._type == EOF

--  private Token peek() {
--    return tokens.get(current);
--  }
peek :: Parser Token
peek = do
    cs <- get
    return $ cs.tokens !! cs.current

--  private Token previous() {
--    return tokens.get(current - 1);
--  }
previous :: Parser Token
previous = do
    currentState <- get
    let prev = currentState.tokens !! (currentState.current - 1)
    return prev
