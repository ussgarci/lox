module Expr (
    Expr (..),
)
where

import StatefulScanner (Literal)
import Token (TokenType)

data Expr
    = Binary {operator :: TokenType, left :: Expr, right :: Expr}
    | Grouping {expression :: Expr}
    | Literal {value :: Literal}
    | Unary {operator :: TokenType, right :: Expr}
    deriving (Show, Eq)
