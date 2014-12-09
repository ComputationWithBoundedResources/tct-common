module Tct.Common.Answer 
  (Answer
  , answering
  ) where

import Tct.Core.Data hiding (Unknown)
import qualified Tct.Core.Common.Pretty as PP

-- | Standard answer type.
data Answer
  = Yes (Complexity, Complexity)
  | Unknown
  | No 
  deriving Show

instance PP.Pretty Answer where
  pretty (Yes (lb, ub)) = PP.text "YES" PP.<> PP.tupled [PP.pretty lb, PP.pretty ub]
  pretty Unknown        = PP.text "MAYBE"
  pretty No             = PP.text "NO"

-- | Returns the time upper bound as an answer.
answering :: Return (ProofTree l) -> SomeAnswer
answering = answer . returning (cert . certificate) (const Unknown)
  where cert c = Yes (timeLB c, timeUB c)

