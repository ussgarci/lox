{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}

module AstPrinter (
    print,
)
where

import Expr qualified as E
import Prelude hiding (print)

print :: E.Expr -> String
print = undefined

-- testor :: E.Expr
-- testor = E.Bin E.Div True True
