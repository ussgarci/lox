{-# LANGUAGE GADTSyntax #-}


module AstPrinter (
    print,
)
where

import qualified Expr as E
import Prelude

--testor :: E.Expr
--testor = E.Bin E.Plus (E.Lit (E.Number 5)) (E.Lit (E.Number 3))

pp :: E.Expr -> IO()
pp (E.Lit x) = ppLiteral x

ppLiteral :: E.Literal -> IO ()
ppLiteral (E.Number x) = print x
ppLiteral (E.String x) = print x
ppLiteral E.True = print "True"
ppLiteral E.False = print "False"
ppLiteral E.Nil = print "Nihil"

