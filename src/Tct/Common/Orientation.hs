module Tct.Common.Orientation
  (
  OrientationProof (..)
  ) where


import qualified Tct.Core.Common.Pretty as PP


data OrientationProof o
  = Order o
  | Incompatible
  | Empty
  | Inapplicable String
  deriving Show

instance PP.Pretty o => PP.Pretty (OrientationProof o) where
  pretty (Order o)        = PP.pretty o
  pretty Incompatible     = PP.paragraph "The input can not be schown compatible."
  pretty (Inapplicable s) = 
    PP.paragraph "The processor is inapplicable, the reason is" PP.<+> PP.paragraph s PP.<> PP.dot
  pretty Empty            = PP.paragraph "All components are processed, nothing ruther to orient."

