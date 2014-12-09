-- | Some handy combinators for proofs.
{-# LANGUAGE DeriveFunctor #-}
module Tct.Common.ProofCombinators 
 ( 
 OrientationProof (..)
 , ApplicationProof (..)
 ) where


import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import qualified Tct.Core.Common.Pretty as PP


data OrientationProof o
  = Order o
  | Empty
  | Incompatible
  deriving (Show, Functor)

instance PP.Pretty o => PP.Pretty (OrientationProof o) where
  pretty (Order o)        = PP.pretty o
  pretty Incompatible     = PP.paragraph "The input can not be schown compatible."
  pretty Empty            = PP.paragraph "All components are processed, nothing ruther to orient."


-- | A 'Either' like type, where Left is fixed to String.
data ApplicationProof p
  = Inapplicable String
  | Applicable p
  deriving (Show, Functor)

newtype ApplicationProofT m p = ApplicationT { runApplicationT :: m (ApplicationProof p) }

-- | Either like data-type where we fix the Left/Error type to String.
instance Applicative ApplicationProof where
  pure                   = Applicable
  Inapplicable msg <*> _ = Inapplicable msg
  Applicable f <*> a     = fmap f a

instance PP.Pretty p => PP.Pretty (ApplicationProof p) where
  pretty (Inapplicable s) = PP.paragraph $ "The processor is not applicable. The reason is " ++ s ++ "."
  pretty (Applicable p)   = PP.pretty p

instance Monad ApplicationProof where
  return                 = Applicable
  Inapplicable msg >>= _ = Inapplicable msg
  Applicable p >>= k     = k p

instance Functor f => Functor (ApplicationProofT f) where
  fmap f = ApplicationT . fmap (fmap f) . runApplicationT

instance (Functor a, Monad a) => Applicative (ApplicationProofT a) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ApplicationProofT m) where
  return = ApplicationT . return . return
  m >>= k = ApplicationT $ do
    a <- runApplicationT m
    case a of
      Inapplicable msg -> return (Inapplicable msg)
      Applicable p     -> runApplicationT (k p)

instance MonadTrans ApplicationProofT where
  lift = ApplicationT . liftM Applicable

instance MonadIO m => MonadIO (ApplicationProofT m) where
  liftIO = lift . liftIO

