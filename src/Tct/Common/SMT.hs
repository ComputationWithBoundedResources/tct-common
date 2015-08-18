module Tct.Common.SMT
  (
  module SMT
  , minismt, minismt'
  , yices, yices'
  , z3, z3'
  , smtSolveTctM
  -- encoding
  , encodePoly
  ) where


import qualified Control.Exception as E (bracket)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Data.List                  (nub)
import           System.Exit
import           System.IO                  (hClose, hFlush, hSetBinaryMode)
import           System.IO.Temp             (openTempFile)
import           System.Process

import           SLogic.Smt                 as SMT hiding (minismt, minismt', yices, yices', z3, z3')

-- import           Tct.Core.Common.Concurrent
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


-- TODO: MS: spawns a process and then pipes the input
-- this seems to be a problem for eg epostar as computing the formula can take its time check if it is better to use
-- the solver of the SLogic library which use temporary files - install Signal handler for WairForProcess error
-- smtSolver :: MonadIO m => Cmd -> Args -> (t -> DiffFormat) -> (String -> Result v) -> t -> m (Result v)
-- smtSolver cmd args formatter parser st = do
--   errM <- liftIO $ spawn cmd args (`hPutDiffFormat` formatter st)
--   return $ either SMT.Error parser errM


smtSolver :: MonadIO m => String -> Cmd -> Args -> (t -> DiffFormat) -> (String -> Result v) -> t -> m (Result v)
smtSolver tmp cmd args formatter parser st = do
  let input = formatter st
  liftIO . withFile $ \file hfile -> do
    hSetBinaryMode hfile True
    -- hSetBuffering hfile BlockBuffering
    hPutDiffFormat hfile input
    hFlush hfile
    hClose hfile
    (code, stdout, stderr) <- readProcessWithExitCode cmd (args ++ [file]) ""
    return $ case code of
      ExitFailure i -> Error $ "Error(" ++ show i ++ "," ++ show stderr ++ ")"
      ExitSuccess   -> parser stdout
    where withFile = E.bracket (openTempFile tmp "smt2x") (hClose . snd) . uncurry


smtSolveTctM :: (Var v, Storing v) => prob -> SmtSolver T.TctM v
smtSolveTctM p st = do
  mso <- T.solver `fmap` T.askState
  mto <- T.remainingTime `fmap` T.askStatus p
  tmp <- T.tempDirectory `fmap` T.askState
  case mso of
    Just (cmd,args)
      | cmd == "minismt" -> minismt' tmp (nub $ ["-m", "-v2", "-neg"] ++ args) mto  st
      | cmd == "yices"   -> yices' tmp args st
      | cmd == "z3"      -> z3' tmp (nub $ ["-smt2", "-in"] ++ args) mto st
      | otherwise        -> defl tmp mto
    Nothing -> defl tmp mto
    where defl tmp mto = minismt' tmp ["-m", "-v2", "-neg"] mto st

minismt' :: (MonadIO m, Storing v, Var v) => String -> Args -> Maybe Int -> SmtSolver m v
minismt' tmp args mto = smtSolver tmp "minismt" (args++to) minismtFormatter minismtParser
  where to = maybe [] (\i -> ["-t", show (max 1 i) ]) mto

minismt :: (MonadIO m, Storing v, Var v) => String -> Maybe Int -> SmtSolver m v
minismt tmp = minismt' tmp ["-m", "-v2", "-neg"]

yices' :: (MonadIO m, Storing v, Var v) => String -> Args -> SmtSolver m v
yices' tmp args = smtSolver tmp "yices-smt2" args yicesFormatter yicesParser

yices :: (MonadIO m, Storing v, Var v) => String -> SmtSolver m v
yices tmp = yices' tmp []

z3' :: (MonadIO m, Storing v, Var v) => String -> Args -> Maybe Int -> SmtSolver m v
z3' tmp args mto = smtSolver tmp "z3" (args++to) z3Formatter z3Parser
  where to = maybe [] (\i -> ["-T", show (max 1 i) ]) mto

z3 :: (MonadIO m, Storing v, Var v) => String -> Maybe Int -> SmtSolver m v
z3 tmp = z3' tmp ["-smt2", "-in"]

-- | standard polynomial encoding
encodePoly :: Ord v => P.Polynomial Int v -> IExpr v
encodePoly ms = SMT.bigAdd (map encodeMono $ P.toView ms) where
  encodeMono (c,ps) = SMT.bigMul (SMT.num c: concatMap encodePower ps)
  encodePower (v,e) = replicate e (SMT.ivar v)

