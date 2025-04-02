module AstPrinter (
    ppExpr,
)
where

import qualified Expr as E
import Prelude

testExpr :: E.Expr
testExpr = E.Bin E.Plus (E.Lit (E.Number 5)) (E.Lit (E.Number 3))

ppExpr :: E.Expr -> String
ppExpr (E.Lit lit) = ppLiteral lit
ppExpr (E.Una op x) = ppUnaryOp op ++ "(" ++ ppExpr x ++ ")"
ppExpr (E.Bin op l r) = "(" ++ ppExpr l ++ " " ++ ppOp op ++ " " ++ ppExpr r ++ ")"

ppLiteral :: E.Lit -> String
ppLiteral (E.Number n) = show n
ppLiteral (E.String s) = "\"" ++ s ++ "\""
ppLiteral E.True = "True"
ppLiteral E.False = "False"
ppLiteral E.Nil = "Nihil"

ppUnaryOp :: E.UnaryOp -> String
ppUnaryOp E.UnaryNegate = "-"
ppUnaryOp E.UnaryBang = "!"

ppOp :: E.Op -> String
ppOp E.Div = "/"
ppOp E.Plus = "+"
ppOp E.Minus = "-"
ppOp E.Times = "*"
ppOp E.Equals = "=="
ppOp E.NotEquals = "!="
ppOp E.LessThan = "<"
ppOp E.GreaterThan = ">"
ppOp E.LessThanEq = "<="
ppOp E.GreaterThanEq = ">="
