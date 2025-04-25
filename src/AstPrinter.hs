module AstPrinter (
    ppExpr,
)
where

import qualified Expr as E
import Prelude

testExpr :: E.Expr
testExpr = E.Bin E.Plus (E.Lit (E.Number 5)) (E.Lit (E.Number 3))

ppExpr :: E.Expr -> String
ppExpr (E.Lit lit) = show lit
ppExpr (E.Una op x) = show op ++ "(" ++ ppExpr x ++ ")"
ppExpr (E.Bin op l r) = "(" ++ ppExpr l ++ " " ++ show op ++ " " ++ ppExpr r ++ ")"
