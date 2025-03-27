
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}


module AstPrinter (
    print
)
where

import Prelude hiding (print)
import Expr qualified as E

print :: E.Expr -> String
print = undefined

--testor :: E.Expr
--testor = E.Bin E.Div True True

