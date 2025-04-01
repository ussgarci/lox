{-# LANGUAGE GADTSyntax #-}

module Expr (
    Expr (..),
    Literal (..),
    UnaryOp (..),
    Op (..),
)
where

data Expr where
    Lit :: Literal -> Expr
    --Una :: UnaryOp -> Expr
    --Bin :: Op -> Expr -> Expr -> Expr
    deriving (Eq, Show)

data Literal where
    Number :: Integer -> Literal
    String :: String -> Literal
    True :: Literal
    False :: Literal
    Nil :: Literal
    deriving (Eq, Show)

data UnaryOp where
    UnaryNegate :: UnaryOp
    UnaryBang :: UnaryOp
    deriving (Eq, Show)

data Op where
    Div :: Op
    Plus :: Op
    Minus :: Op
    Times :: Op
    --Equals :: Op
    --NotEquals :: Op
    --LessThan :: Op
    --GreaterThan :: Op
    --LessThanEq :: Op
    --GreaterThanEq :: Op
    deriving (Show, Eq)
