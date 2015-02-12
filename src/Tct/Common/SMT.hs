module Tct.Common.SMT
  (
  module SMT
  , minismt, minismt'
  , yices, yices'
  , z3, z3'
  ) where


import           Control.Applicative        ((<$>))
import qualified Data.ByteString            as BS
import qualified Data.Map                   as M

import           SLogic.Smt                 as SMT hiding (minismt, minismt', yices, yices', z3, z3')

import           Tct.Core.Common.Concurrent

import           Tct.Common.Ring


instance Additive SMT.IExpr where
  zero = SMT.zero
  add  = (SMT..+)

instance Multiplicative SMT.IExpr where
  one = SMT.one
  mul = (SMT..*)

instance AdditiveGroup SMT.IExpr where
  neg = SMT.neg


smtSolver :: Cmd -> Args -> (t -> BS.ByteString) -> (String -> Result v) -> t -> IO (Result v)
smtSolver cmd args formatter parser st =
  either SMT.Error parser <$> spawn cmd args (`BS.hPutStr` formatter st)

minismt' :: Args -> SolverState Expr -> IO (Result (M.Map Var Value))
minismt' args = smtSolver "minismt" args minismtFormatter minismtParser

minismt :: SolverState Expr -> IO (Result (M.Map Var Value))
minismt = minismt' ["-m", "-v2", "-neg"]

yices' :: Args -> SolverState Expr -> IO (Result (M.Map Var Value))
yices' args = smtSolver "yices-smt2" args yicesFormatter yicesParser

yices :: SolverState Expr -> IO (Result (M.Map Var Value))
yices = yices' []

z3' :: Args -> SolverState Expr -> IO (Result (M.Map Var Value))
z3' args = smtSolver "z3" args z3Formatter z3Parser

z3 :: SolverState Expr -> IO (Result (M.Map Var Value))
z3 = z3' ["-smt2", "-in"]

