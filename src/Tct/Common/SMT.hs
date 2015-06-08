module Tct.Common.SMT
  (
  module SMT
  , minismt, minismt'
  , yices, yices'
  , z3, z3'
  , smtSolve
  -- encoding
  , encodePoly
  ) where


import           Control.Monad.Trans        (MonadIO, liftIO)
import qualified Data.ByteString            as BS
import           Data.List                  (nub)
import qualified Data.Map                   as M

import           SLogic.Smt                 as SMT hiding (minismt, minismt', yices, yices', z3, z3')

import           Tct.Core.Common.Concurrent
import qualified Tct.Core.Data              as T

import qualified Tct.Common.Polynomial      as P
import           Tct.Common.Ring


instance Additive SMT.IExpr where
  zero = SMT.zero
  add  = (SMT..+)

instance Multiplicative SMT.IExpr where
  one = SMT.one
  mul = (SMT..*)

instance AdditiveGroup SMT.IExpr where
  neg = SMT.neg


smtSolver :: MonadIO m => Cmd -> Args -> (t -> BS.ByteString) -> (String -> Result v) -> t -> m (Result v)
smtSolver cmd args formatter parser st = do
  errM <- liftIO $ spawn cmd args (`BS.hPutStr` formatter st)
  return $ either SMT.Error parser errM

smtSolve :: p -> SolverState Expr -> T.TctM (Result (M.Map Var Value))
smtSolve p st = do
  mso <- T.solver `fmap` T.askState
  mto <- T.remainingTime `fmap` T.askStatus p
  case mso of
    Just (cmd,args)
      | cmd == "minismt" -> minismt' (nub $ ["-m", "-v2", "-neg"] ++ args) mto  st
      | cmd == "yices"   -> yices' args st
      | cmd == "z3"      -> z3' (nub $ ["-smt2", "-in"] ++ args) mto st
      | otherwise        -> defl mto
    Nothing -> defl mto
    where defl mto = minismt' ["-m", "-v2", "-neg"] mto st

minismt' :: MonadIO m => Args -> Maybe Int -> SolverState Expr -> m (Result (M.Map Var Value))
minismt' args mto = smtSolver "minismt" (args++to) minismtFormatter minismtParser
  where to = maybe [] (\i -> ["-t", show (max 1 i) ]) mto

minismt :: MonadIO m => Maybe Int -> SolverState Expr -> m (Result (M.Map Var Value))
minismt = minismt' ["-m", "-v2", "-neg"]

yices' :: MonadIO m => Args -> SolverState Expr -> m (Result (M.Map Var Value))
yices' args = smtSolver "yices-smt2" args yicesFormatter yicesParser

yices :: MonadIO m => SolverState Expr -> m (Result (M.Map Var Value))
yices = yices' []

z3' :: MonadIO m => Args -> Maybe Int -> SolverState Expr -> m (Result (M.Map Var Value))
z3' args mto = smtSolver "z3" (args++to) z3Formatter z3Parser
  where to = maybe [] (\i -> ["-T", show (max 1 i) ]) mto

z3 :: MonadIO m => Maybe Int -> SolverState Expr -> m (Result (M.Map Var Value))
z3 = z3' ["-smt2", "-in"]

-- | standard polynomial encoding
encodePoly :: P.Polynomial Int String -> SMT.IExpr
encodePoly ms = SMT.bigAdd (map encodeMono $ P.toView ms) where
  encodeMono (c,ps) = SMT.bigMul (SMT.num c: concatMap encodePower ps)
  encodePower (v,e) = replicate e (SMT.ivar v)

