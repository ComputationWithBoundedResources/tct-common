-- | This module provides abstract polynomial interpretations.
module Tct.Common.PolynomialInterpretation
  (
  Shape (..)
  , shapeArg
  , Kind (..)
  , SomeIndeterminate
  , indeterminates
  , SomePolynomial

  , CoefficientVar(..)
  , PolyInter (..)

  , mkInterpretation
  , degree
  ) where


import Data.Monoid
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Data (Typeable)

import qualified Tct.Core.Common.Pretty as PP
import qualified Tct.Core.Common.Parser as P
import Tct.Core.Data 

import qualified Tct.Common.Polynomial  as P


-- | The shape of the polynomials.
data Shape
  = StronglyLinear
  | Linear
  | Quadratic
  | Mixed Int
  deriving (Eq, Show, Typeable)

shapeArg :: Argument Required Shape
shapeArg = arg { argName = "shape" , argDomain = "<shape>" }  `withHelp` (s1:shapes)
  where
    s1 = "Specifies the shape of the polynomial. <shape> is one of:"
    shapes = map ('*':)  [ show StronglyLinear, show Linear, show Quadratic, "Mixed <nat>"]

instance SParsable prob Shape where
  parseS = P.choice
    [ P.symbol (show StronglyLinear) >> return StronglyLinear
    , P.symbol (show Linear)         >> return Linear
    , P.symbol (show Quadratic)      >> return Quadratic
    , P.symbol "Mixed "              >> P.nat >>= return . Mixed ]


-- | The kind of the interpretation.
data Kind fun
  = Unrestricted     { shape :: Shape }
  | ConstructorBased { shape :: Shape, constructors :: S.Set fun }
  deriving Show

-- | Canonical variable type for abstract polynomials.
newtype SomeIndeterminate = SomeIndeterminate Int deriving (Eq, Ord, Enum)

-- | By convention we start with 1.
indeterminates :: [SomeIndeterminate]
indeterminates = [SomeIndeterminate 1 .. ]


type SomePolynomial c = P.Polynomial c SomeIndeterminate

-- | Coefficients of the abstract polynomial.
data CoefficientVar fun = CoefficientVar
  { restrict :: Bool                        -- ^ Strictness Annotation.
  , varfun   :: fun
  , argpos   :: P.MView SomeIndeterminate
  } deriving (Eq, Ord, Show)

newtype PolyInter fun c = PolyInter 
  { interpretations :: M.Map fun (SomePolynomial c) }
  deriving Show

mkCoefficient :: Ord fun => Kind fun -> fun -> P.MView SomeIndeterminate -> CoefficientVar fun
mkCoefficient (Unrestricted shp) f        = CoefficientVar (shp == StronglyLinear) f
mkCoefficient (ConstructorBased shp cs) f = CoefficientVar (shp == StronglyLinear || f `S.member` cs) f

-- FIXME: MS: for constructorbased; we restict the coefficient but not the shape !!!
mkInterpretation :: Ord fun => Kind fun -> (fun, Int) -> P.PView (CoefficientVar fun) SomeIndeterminate
mkInterpretation k (f,ar) = fromShape (shape k) (mkCoefficient k f) (take ar indeterminates)
  where
    fromShape StronglyLinear = P.linear
    fromShape Linear         = P.linear
    fromShape Quadratic      = P.quadratic
    fromShape (Mixed i)      = P.mixed i

degree :: Ord fun => Kind fun -> PolyInter fun Int -> Complexity
degree k inter = case k of
  Unrestricted {}
    | deg1 && isStrong -> Poly (Just 1)
    | deg1             -> Exp (Just 1)
    | otherwise        -> Exp (Just 2)
    where
      deg1     = M.foldr' (\p b -> (P.degree p <= 1 && b)) True inters
      isStrong = M.foldr' ((&&) . P.isStronglyLinear) True inters
  ConstructorBased _ cs -> Poly (Just deg)
    where deg = M.foldrWithKey' (\f p b -> max b $ if f `S.member` cs then 0 else P.degree p) 0 inters
  where inters = interpretations inter


--- Proofdata --------------------------------------------------------------------------------------------------------

instance Show SomeIndeterminate where
  show (SomeIndeterminate i) = "x" ++ show i

instance PP.Pretty SomeIndeterminate where
  pretty (SomeIndeterminate i) = PP.text "x" <> PP.int i

instance PP.Pretty Shape where
  pretty StronglyLinear = PP.text "stronglyLinear"
  pretty Linear         = PP.text "linear"
  pretty Quadratic      = PP.text "quadratic"
  pretty (Mixed i)      = PP.text "mixed" <> PP.parens (PP.int i)

instance PP.Pretty fun => PP.Pretty (Kind fun) where
  pretty (Unrestricted shp)       = PP.text "unrestricted" <> PP.parens (PP.pretty shp)
  pretty (ConstructorBased shp _) = PP.text "constructor-based" <> PP.parens (PP.pretty shp)

instance PP.Pretty fun => PP.Pretty (PolyInter fun Int) where
  pretty pint = PP.table [(PP.AlignRight, as), (PP.AlignLeft, bs), (PP.AlignLeft,cs)]
    where 
      (as,bs,cs) = unzip3 $ map k (M.toList $ interpretations pint)
      k (f,p)    = (PP.text "p" <> PP.parens (PP.pretty f), PP.text " = ", PP.pretty p)

