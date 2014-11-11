module Tct.Common.Answer 
  (Answer
  , answering
  ) where

import Tct.Core.Data hiding (Unknown)
import qualified Tct.Core.Common.Pretty as PP

-- | Standard answer type.
data Answer
  = Yes Complexity
  | Unknown
  | No 
  deriving Show

instance PP.Pretty Answer where
  pretty = PP.text . show

-- | Returns the time upper bound as an answer.
answering :: Return (ProofTree l) -> SomeAnswer
answering = answer . returning (Yes . timeUB . certificate) (const Unknown)

