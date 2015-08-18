{-# LANGUAGE FlexibleContexts #-}
-- | This module re-exports SLogic.Smt and provides specialised commands for invoking the solver.
module Tct.Common.SMT
  (
  module SMT
  , smtSolveTctM
  , minismt, minismt'
  , yices, yices'
  , z3, z3'
  -- encoding
  , encodePoly
  ) where


import           Control.Exception          (bracket)
import           Control.Monad.Error        (throwError)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Data.List                  (nub)
import           Data.Maybe                 (fromMaybe)
import           System.IO                  (hClose, hFlush, hSetBinaryMode)
import           System.IO.Temp             (openTempFile)

import           SLogic.Smt                 as SMT hiding (minismt, minismt', yices, yices', z3, z3')

import           Tct.Core.Common.Concurrent
import qualified Tct.Core.Data              as T

import qualified Tct.Common.Polynomial      as P
import           Tct.Common.Ring


instance Additive (IExpr v) where
  zero = SMT.zero
  add  = (SMT..+)

instance Multiplicative (IExpr v) where
  one = SMT.one
  mul = (SMT..*)

instance AdditiveGroup (IExpr v) where
  neg = SMT.neg


-- | This is the preferred way to invoke a solver in 'TctM'.
-- Invokes the solver specified in the configuration. Currently "minismt", "z3" and "yices" are supported. If the
-- solver is undefined then "minismt" is used.
smtSolveTctM :: (Var v, Storing v) => prob -> SmtSolver T.TctM v
smtSolveTctM p st = do
  mso <- T.solver `fmap` T.askState
  tmp <- T.tempDirectory `fmap` T.askState
  mto <- T.remainingTime `fmap` T.askStatus p
  case mso of
    Just (cmd,args)
      | cmd == "minismt" -> minismt' (Just tmp) mto (nub $ ["-m", "-v2", "-neg"] ++ args) st
      | cmd == "z3"      -> z3' (Just tmp) mto (nub $ "-smt2": args) st
      | cmd == "yices"   -> yices' (Just tmp) args st
      | otherwise        -> defl tmp mto
    Nothing -> defl tmp mto
    where defl tmp mto = minismt' (Just tmp) mto ["-m", "-v2", "-neg"] st

gSolver :: MonadIO m => Maybe FilePath -> Cmd -> Args -> (t -> DiffFormat) -> (String -> Result v) -> t -> m (Result v)
gSolver mtmp cmd args formatter parser st = do
  let tmp = "tmp" `fromMaybe` mtmp
  let input = formatter st
  liftIO . withFile tmp $ \file hfile -> do
    hSetBinaryMode hfile True
    -- hSetBuffering hfile BlockBuffering
    hPutDiffFormat hfile input
    hFlush hfile
    hClose hfile
    either (throwError . userError) (return . parser) =<< spawn' cmd (args ++ [file])
    where withFile tmp = bracket (openTempFile tmp "smt2x") (hClose . snd) . uncurry

-- | minismt solver instance
minismt' :: (MonadIO m, Storing v, Var v)
  => Maybe FilePath -- ^ temporary directory
  -> Maybe Int      -- ^ optional timeout
  -> Args -> SmtSolver m v
minismt' mtmp mto args = gSolver mtmp "minismt" (args++to) minismtFormatter minismtParser
  where to = maybe [] (\i -> ["-t", show (max 1 i) ]) mto

-- | prop> minismt = minismt' Nothing Nothing ["-m", "-v2", "-neg"]
minismt :: (MonadIO m, Storing v, Var v) => SmtSolver m v
minismt = minismt' Nothing Nothing ["-m", "-v2", "-neg"]

-- | yices solver instance
yices' :: (MonadIO m, Storing v, Var v) => Maybe FilePath -> Args -> SmtSolver m v
yices' mtmp args = gSolver mtmp "yices-smt2" args yicesFormatter yicesParser

-- | prop> yices = yices' Nothing []
yices :: (MonadIO m, Storing v, Var v) => SmtSolver m v
yices = yices' Nothing []

-- | z3 solver instance
z3' :: (MonadIO m, Storing v, Var v)
  => Maybe FilePath -- ^ temporary directory
  -> Maybe Int      -- ^ optional timeout
  -> Args -> SmtSolver m v
z3' mtmp mto args = gSolver mtmp "z3" (args++to) z3Formatter z3Parser
  where to = maybe [] (\i -> ["-T:"++ show (max 1 i)]) mto

-- | prop> z3 = z3' Nothing Nothing ["smt2"]
z3 :: (MonadIO m, Storing v, Var v) =>  SmtSolver m v
z3 = z3' Nothing Nothing ["-smt2"]

-- | standard polynomial encoding
encodePoly :: Ord v => P.Polynomial Int v -> IExpr v
encodePoly ms = SMT.bigAdd (map encodeMono $ P.toView ms) where
  encodeMono (c,ps) = SMT.bigMul (SMT.num c: concatMap encodePower ps)
  encodePower (v,e) = replicate e (SMT.ivar v)

