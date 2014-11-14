{- | This module provides a tree-based implementation for multivariate polynomials.

A 'Monomial' is well-defined if:

    * each variable (indeterminate) occurs at most once
    * exponents are >= 1

A 'Polynomial' is well-defined if:

    * monomials are valid
    * monomials are unique
    * coefficients are non-zero wrt. 'zero'


All operations on 'Polynomial' are invariant wrt to well-definedness.

Polynomials can be constructed using the 'constant' and 'variable' functions together with
standard arithmetic ('SemiRing', 'Ring'); or
using 'PolynomialView', together with 'fromView'.


NOTE: 'Ord' and 'Eq' are derived for the 'Polynomial' and 'Monomial' type.
They are purely syntactical and shouldn't be used to eg compare polynomials.
-}
module Tct.Common.Polynomial
  (
  -- * Polynomial
  Monomial
  , Polynomial
  , scale
  -- ** Getters
  , coefficients
  , coefficients'
  , variables
  , constantValue
  , splitConstantValue
  -- ** Properties
  , degree
  , isLinear
  , isStronglyLinear
  -- ** Substitution/Maps
  , mapCoefficients
  , substituteVariables

  -- * View
  , MonomialView (..)
  , PolynomialView (..)
  -- ** view constructors
  , (^^^), monov, polyv
  , linear
  , quadratic
  , mixed

  -- ** polynomial construction
  , constant
  , variable
  , fromView
  , fromViewWith
  , fromViewWithM
  ) where


import           Control.Monad
import           Data.List              (foldl', nub)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe)

import qualified Tct.Core.Common.Pretty as PP

import           Tct.Common.Ring


-- the zero polynomial is represented by (Poly M.empty)
-- a constant v is represented by (Poly (M.singleton (Mono M.empty) c))

-- | The abstract monomial type.
newtype Monomial v = Mono (M.Map v Int)
  deriving (Eq, Ord, Show)

-- we treat negative exponents as 1
mpower :: v -> Int -> Monomial v
mpower v i
  | i <= 0    = Mono (M.singleton v 1)
  | otherwise = Mono (M.singleton v i)

mvariable :: v -> Monomial v
mvariable = flip mpower 1


mone :: Monomial v
mone = Mono M.empty

mmult :: Ord v => Monomial v -> Monomial v -> Monomial v
mmult (Mono ps1) (Mono ps2) = Mono $ M.unionWith (+) ps1 ps2

instance Ord v => Multiplicative (Monomial v) where
  one = mone
  mul = mmult


-- | The abstract polynomial type.
newtype Polynomial c v = Poly (M.Map (Monomial v) c)
  deriving (Eq, Ord, Show)

-- | Constructs a polynomial from a constant value.
constant :: (Additive c, Eq c, Ord v) => c -> Polynomial c v
constant c
  | c == zero = pzero
  | otherwise = Poly (M.singleton mone c)

-- | Constructs a polynomial from a variable (with exponent 1).
variable :: (Multiplicative c, Ord v) => v -> Polynomial c v
variable v = Poly (M.singleton (mvariable v) one)


pnormalise :: (Additive c, Eq c) => Polynomial c v -> Polynomial c v
pnormalise (Poly ts) = Poly $ M.filter (/= zero) ts

pzero :: Polynomial c v
pzero = Poly M.empty

padd :: (Additive c, Eq c, Ord v) => Polynomial c v -> Polynomial c v -> Polynomial c v
padd (Poly ts1) (Poly ts2) = pnormalise . Poly $ M.unionWith add ts1 ts2

instance (Additive c, Eq c, Ord v) => Additive (Polynomial c v) where
  zero = pzero
  add  = padd


asConstant :: (SemiRing c, Eq v) => Polynomial c v -> Maybe c
asConstant (Poly ts)
  | M.size ts > 1 = Nothing
  | otherwise     = case M.toList ts of
    []                  -> Just zero
    [(m,c)] | m == mone -> Just c
    _                   -> Nothing

pone :: (Multiplicative c, Ord v) => Polynomial c v
pone = Poly (M.singleton mone one)

-- | Multiplies a constant value with a polynomial.
scale :: (SemiRing c, Eq c) => c -> Polynomial c v -> Polynomial c v
scale c (Poly ts)
  | c == zero = pzero
  | otherwise = Poly $ M.map (`mul` c) ts

pmult :: (SemiRing c, Eq c, Ord v) => Polynomial c v -> Polynomial c v -> Polynomial c v
pmult p1@(Poly ts1) p2@(Poly ts2) = case (asConstant p1, asConstant p2) of
  (Just c1, Just c2) -> constant (c1 `mul` c2)
  (Just c, _)        -> scale c p2
  (_, Just c)        -> scale c p1
  _                  ->
    Poly $ M.fromListWith add
      [ (m1 `mmult` m2, c1 `mul` c2) | (m1,c1) <- M.toList ts1, (m2,c2) <- M.toList ts2 ]

instance (SemiRing c, Eq c, Ord v) => Multiplicative (Polynomial c v) where
  one = pone
  mul = pmult

pnegate :: AdditiveGroup c => Polynomial c v -> Polynomial c v
pnegate (Poly ts) = Poly (M.map neg ts)

instance (AdditiveGroup c, Eq c, Ord v) => AdditiveGroup (Polynomial c v) where
  neg = pnegate


--- * Getters --------------------------------------------------------------------------------------------------------

-- | Returns the coefficients of the polynomial (in arbitrary order). Never returns an empty list.
--
-- prop> coefficients (constant zero) = [zero]
coefficients :: (SemiRing c, Ord v) => Polynomial c v -> [c]
coefficients p@(Poly ts)
  | isZero p  = [zero]
  | otherwise = M.elems ts

-- | Like 'coefficients', but separates the constant part.
-- 
-- prop> coefficients' (constant v) = ([], v)
coefficients' :: (SemiRing c, Ord v) => Polynomial c v -> ([c],c)
coefficients' p = (M.elems ts, c)
  where (Poly ts,c) = splitConstantValue p

-- | Returns the (set of) variables of the polynomial (in arbitrary order).
--
-- prop> variables (constant v) = []
variables :: Eq v => Polynomial c v -> [v]
variables (Poly ts) = nub $ concatMap k (M.keys ts)
  where k (Mono ps) = M.keys ps

-- | Returns the constant value of the polynomial.
--
-- prop> constantValue zero = zero
-- prop> constantValue (constant v) = v
constantValue :: (SemiRing c, Ord v) => Polynomial c v -> c
constantValue (Poly ts) = zero `fromMaybe` M.lookup mone ts

-- | Splits a non-zero polynomial in a (non-constant, constant) part.
--
-- prop> splitConstantValue (constant zero) = (zero,zero)
splitConstantValue :: (SemiRing c, Ord v) => Polynomial c v -> (Polynomial c v, c)
splitConstantValue p@(Poly ts)
  | isZero p  = (pzero, zero)
  | otherwise = (Poly (M.filterWithKey f ts), constantValue p)
    where f k _ = k /= mone




--- * Properties -----------------------------------------------------------------------------------------------------

isZero :: Polynomial c v -> Bool
isZero (Poly ts) = M.null ts

mdegree :: Monomial v -> Int
mdegree (Mono ps) = sum (M.elems ps)

-- | Returns the degree of the polynomial.
--
-- prop> degree zero = -1
-- prop> degree (constant v) = 0
degree :: (Additive c, Eq c, Ord v) => Polynomial c v -> Int
degree p@(Poly ts)
  | isZero p  = -1
  | otherwise = maximum (map mdegree $ M.keys ts)

misLinear :: Monomial v -> Bool
misLinear m = mdegree m <= 1

-- | Checks if the polynomial is linear.
isLinear :: Polynomial c v -> Bool
isLinear (Poly ts) = all misLinear (M.keys ts)

-- | Checks if the polynomial is strongly linear.
isStronglyLinear :: (SemiRing c, Eq c, Ord v) =>  Polynomial c v -> Bool
isStronglyLinear (Poly ts) = all k (M.toList ts)
  where
    k (m, c)
      | m == mone = True
      | otherwise = c == one && misLinear m


pbigMult :: (SemiRing c, Eq c, Ord v) => [Polynomial c v] -> Polynomial c v
pbigMult = foldl' pmult pone

pbigAdd :: (SemiRing c, Eq c, Ord v) => [Polynomial c v] -> Polynomial c v
pbigAdd = foldl' padd pzero

-- | @'substituteVariables' p subs@ substitutes the variables in p according to @subs@.
-- Variables occuring not in @subs@ are mapped to the unit ('one') polynomial.
substituteVariables :: (SemiRing c, Eq c, Ord v, Ord v') => Polynomial c v -> M.Map v (Polynomial c v') -> Polynomial c v'
substituteVariables (Poly ts) subs = pbigAdd $ foldl' handleTerms [] (M.toList ts)
  where
    handleTerms polys (m,c) = (c `scale` handleMono m) : polys
    subs' = M.toList subs
    handleMono (Mono ps) = pbigMult $ foldl' k [] subs'
      where
        k polys (v,p) = case M.lookup v ps of
          Just i  -> polys ++ replicate i p
          Nothing -> polys

-- | Maps over the coefficients.
mapCoefficients :: (Additive c', Eq c') => (c -> c') -> Polynomial c v -> Polynomial c' v
mapCoefficients f (Poly ts) = pnormalise $ Poly (f `M.map` ts)


--- * View -----------------------------------------------------------------------------------------------------------

-- | Power type with variable @v@.
data PowerView v         = PowV v Int deriving (Eq, Ord, Show)
-- | Monomial type with coefficient @c@ and variable @v@.
data MonomialView v      = MonoV [PowerView v] deriving (Eq, Ord, Show)
-- | Polynomial type with coefficient @c@ and variable @v@.
newtype PolynomialView c v = PolyV [(c,Monomial v)] deriving (Eq, Ord, Show)

-- | > v^^^1 = PowV v i
(^^^) :: v -> Int -> PowerView v
v^^^i = PowV v i

-- | prop> mono = MonoV
monov :: Ord v => [PowerView v] -> MonomialView v
monov = MonoV

-- | prop> poly = PolyV
polyv :: [(c,Monomial v)] -> PolynomialView c v
polyv = PolyV


-- | Like 'fromViewWith' with the identity function applied.
--
-- prop> fromView = fromViewWith id
fromView :: (Additive c, Eq c, Ord v) => PolynomialView c v -> Polynomial c v
fromView = fromViewWith id

-- | 'fromViewWith f p' applies @f@ to all coefficients and constructs a polynomial from a view.
fromViewWith :: (Additive c', Eq c', Ord v) => (c -> c') -> PolynomialView c v -> Polynomial c' v
fromViewWith f (PolyV ts) = Poly $ foldl' k M.empty ts where
  k p (c, m) = let c' = f c in
    if c' /= zero then M.insertWith add m c' p else p

-- | Monadic version of 'fromViewWith'.
fromViewWithM :: (Monad m, Additive c', Eq c', Ord v) => (c -> m c') -> PolynomialView c v -> m (Polynomial c' v)
fromViewWithM f (PolyV ts) = Poly `liftM` foldM k M.empty ts where
  k p (c,m) = do
    c' <- f c
    return $ if c' /= zero
      then M.insertWith add m c' p
      else p


-- | Lifts a constant to a polynom.
--constantv :: c -> PolynomialView c v
--constantv c = PolyV [(c,mone)]

-- | Lifts a variable to a polynom (with exponent 1).
--variablev :: Multiplicative c => v -> PolynomialView c v
--variablev v = PolyV [(one, Mono $ M.singleton v 1)]

mfromView :: Ord v => MonomialView v -> Monomial v
mfromView (MonoV ps) = Mono $ foldl' k M.empty ps
  where k m (PowV v i) = if i > 0 then M.insertWith (+) v i m else m

-- | @'linear' f [x,...,z] = cx*x + ... + cz*z + c@
-- constructs a linear polynomial; the coefficients are determinded by applying @f@ to each monomial.
linear :: Ord v => (Monomial v -> c) -> [v] -> PolynomialView c v
linear f = polyv . (mkTerm [] :) . map (\v -> mkTerm [v^^^1])
  where mkTerm ps = let m = mfromView (MonoV ps) in (f m,m)

-- | @'quadratic' f [x,...,z] = cx2*x^2 + cx*x + ... + cz2*z^2 + cz*z + c@
-- constructs a quadratic polynomial; the coefficients are determined by applying @f@ to each monomial.
quadratic :: Ord v => (Monomial v -> c) -> [v] -> PolynomialView c v
quadratic f = polyv . (mkTerm [] :) . map (\v -> mkTerm [v^^^2,v^^^1])
  where mkTerm ps = let m = mfromView (MonoV ps) in (f m, m)

-- | Creates a mixed polynom up to a specified degree; the coefficients are determined by applying @f@ to each monomial.
--
-- > mixed 2 (const 1) "xz" = x^2 + x*z + x + z^2 + z + 1
mixed :: Ord v => Int -> (Monomial v -> c) -> [v] -> PolynomialView c v
mixed d f vs =  polyv $ map mkTerm pows
  where
    mkTerm ps = let m = mfromView (MonoV ps) in (f m, m)
    pows =
      map (filter (\(PowV _ i) -> i>0) . zipWith PowV vs) -- [] isElem of pows
      . filter (\ps -> sum ps <= d)
      . sequence $ replicate (length vs) [0..d]


--- * Pretty Printing ------------------------------------------------------------------------------------------------
instance PP.Pretty v => PP.Pretty (Monomial v) where
  pretty = ppMonomial PP.pretty

instance (Multiplicative c, Eq c, PP.Pretty c, PP.Pretty v) => PP.Pretty (Polynomial c v) where
  pretty = ppPolynomial PP.pretty PP.pretty

ppMonomial :: (v -> PP.Doc) -> Monomial v -> PP.Doc
ppMonomial pp (Mono ps)
  | M.null ps = PP.empty
  | otherwise = PP.hcat . PP.punctuate (PP.char '*') . map k $ M.toAscList ps
    where k (v,i) = pp v PP.<> if i == 1 then PP.empty else PP.char '^' PP.<> PP.int i

ppPolynomial  :: (Multiplicative c, Eq c) => (c -> PP.Doc) -> (v -> PP.Doc) -> Polynomial c v -> PP.Doc
ppPolynomial ppr ppv (Poly ts)
  | M.null ts = PP.int 0
  | otherwise = PP.hcat . PP.punctuate (PP.text " + ") $ map k $ M.toAscList ts
  where
    k (m@(Mono ps), c)
      | M.null ps = ppr c
      | c == one  = ppMonomial ppv m
      | otherwise = ppr c PP.<> PP.char '*' PP.<> ppMonomial ppv m

