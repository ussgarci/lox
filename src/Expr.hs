{-# LANGUAGE GADTSyntax #-}

module Expr (
    Expr (..),
    Lit (..),
    UnaryOp (..),
    Op (..),
)
where

data Expr where
    Lit :: Lit -> Expr
    Una :: UnaryOp -> Expr -> Expr
    Bin :: Op -> Expr -> Expr -> Expr
    deriving (Eq, Show)

data Lit where
    Number :: Integer -> Lit
    String :: String -> Lit
    True :: Lit
    False :: Lit
    Nil :: Lit
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
    Equals :: Op
    NotEquals :: Op
    LessThan :: Op
    GreaterThan :: Op
    LessThanEq :: Op
    GreaterThanEq :: Op
    deriving (Show, Eq)
