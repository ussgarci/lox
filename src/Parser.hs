{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parser (
    parse,
    ParserState (..),
)
where

import Control.Monad.Extra (ifM, whenM)
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
-- primary        → number | string | "true" | "false" | "nil"
--                | "(" expression ")" ;

type Parser a = State ParserState a

parse :: Parser Expr
parse = do
    expression

expression :: Parser Expr
expression = equality

-- equality       → comparison ( ( "!=" | "==" ) comparison )* ;
equality :: Parser Expr
equality = do
    expr <- comparison
    whileM_
        ( do
            match [BANG_EQUAL, EQUAL_EQUAL]
        )
        ( do
            operator <- previous
            return $ Binary expr operator <$> comparison
        )
    return expr

-- comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
comparison :: Parser Expr
comparison = do
    expr <- term
    whileM_
        ( do
            match [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]
        )
        ( do
            operator <- previous
            return $ Binary expr operator <$> factor
        )
    return expr

-- term           → factor ( ( "-" | "+" ) factor )* ;
term :: Parser Expr
term = do
    expr <- factor
    whileM_
        ( do
            match [MINUS, PLUS]
        )
        ( do
            operator <- previous
            return $ Binary expr operator <$> factor
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
            return $ Binary expr operator <$> unary
        )
    return expr

-- unary          → ( "!" | "-" ) unary
--                | primary ;
unary :: Parser Expr
unary = do
    ifM
        (match [BANG, MINUS])
        ( do
            operator <- previous
            Unary operator <$> unary
        )
        ( do
            primary
        )

-- primary        → number | string | "true" | "false" | "nil"
--                | "(" expression ")" ;
primary :: Parser Expr
primary = do
    currentToken <- advance
    case currentToken._type of
        NUMBER -> return $ Literal currentToken
        STRING -> return $ Literal currentToken
        FALSE -> return $ Literal currentToken
        TRUE -> return $ Literal currentToken
        NIL -> return $ Literal currentToken
        LEFT_PAREN -> do
            _ <- advance
            expr <- expression
            _ <- advance
            return $ Grouping expr
        _ -> error $ "Unexpected token in primary: " ++ show currentToken

-- private Token advance() {
--   if (!isAtEnd()) current++;
--   return previous();
-- }
advance :: Parser Token
advance = do
    whenM (not <$> isAtEnd) (do modify $ \s -> s{current = s.current + 1})
    previous

--  private boolean check(TokenType type) {
--    if (isAtEnd()) return false;
--    return peek().type == type;
--  }
check :: TokenType -> Parser Bool
check tt = do
    ifM
        isAtEnd
        (return False)
        ( do
            token <- peek
            return $ token._type == tt
        )

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
match [] = return False
match (t : ts) = do
    ifM
        (check t)
        ( do
            _ <- advance
            return True
        )
        (match ts)

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
