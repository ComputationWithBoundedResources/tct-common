module Tct.Common.SMT
  (
  module SMT
  ) where


import SLogic.Smt as SMT 

import Tct.Common.Ring

instance Additive SMT.IExpr where
  zero = SMT.zero
  add  = (SMT..+)

instance Multiplicative SMT.IExpr where
  one = SMT.one
  mul = (SMT..*)

instance AdditiveGroup SMT.IExpr where
  neg = SMT.neg

