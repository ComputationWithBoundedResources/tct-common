module Tct.Common.SMT
  (
  module SMT
  ) where


import SmtLib.Logic.Core      as SMT
import SmtLib.Logic.Int       as SMT
import SmtLib.SMT             as SMT
import SmtLib.Solver          as SMT

import Tct.Common.Ring
import Tct.Core.Common.Pretty as PP


instance Additive SMT.Expr where
  zero = SMT.zero
  add  = (SMT..+)

instance Multiplicative SMT.Expr where
  one = SMT.one
  mul = (SMT..*)

instance AdditiveGroup SMT.Expr where
  neg = SMT.nNeg

instance PP.Pretty SMT.Expr where
  pretty = PP.text . SMT.prettyExpr

