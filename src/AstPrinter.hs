module AstPrinter (
    ppExpr,
)
where

import Expr
import StatefulScanner (Literal (..))
import Token (TokenType (MINUS, STAR))
import Prelude

testExpr :: Expr
testExpr =
    Binary
        { left =
            Unary
                { operator = MINUS
                , right = Literal{value = Number 123.0}
                }
        , operator = STAR
        , right =
            Grouping
                { expression = Literal{value = Number 45.67}
                }
        }

ppExpr :: Expr -> String
ppExpr = undefined
