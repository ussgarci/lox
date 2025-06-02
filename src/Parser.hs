module Parser (
    parse
)
where

import Expr
import StatefulScanner (Token)
import Control.Monad.State

data ParserState = ParserState
    { current :: Int
    , tokens :: [Token]
    }
    deriving (Show)

-- expression     → literal
--                | unary
--                | binary
--                | grouping ;
--
-- literal        → NUMBER | STRING | "true" | "false" | "nil" ;
-- grouping       → "(" expression ")" ;
-- unary          → ( "-" | "!" ) expression ;
-- binary         → expression operator expression ;
-- operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
--                | "+"  | "-"  | "*" | "/" ;

parse :: State ParserState Expr
parse = undefined
