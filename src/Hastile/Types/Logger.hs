{-# LANGUAGE OverloadedStrings #-}
module Hastile.Types.Logger where

import qualified Control.Monad.Logger  as Logger
import qualified Katip
import qualified System.IO             as IO
import qualified System.Log.FastLogger as FastLogger

fromLevel :: LogLevel -> Severity
fromLevel LevelDebug     = DebugS
fromLevel LevelInfo      = InfoS
fromLevel LevelWarn      = WarningS
fromLevel LevelError     = ErrorS
fromLevel (LevelOther _) = NoticeS

defaultLogEnv :: IO LogEnv
defaultLogEnv = do
    handleScribe <- mkHandleScribe ColorIfTerminal IO.stdout DebugS V2
    env <- initLogEnv "hastile" "production"
    registerScribe "stdout" handleScribe defaultScribeSettings env

adapt :: (ToLogStr msg, Applicative m, Katip m)  => (Namespace -> Severity -> Katip.LogStr -> m ()) -> Loc -> LogSource -> LogLevel -> msg -> m ()
adapt f _ src lvl msg =
    f ns (fromLevel lvl) $ logStr' msg
  where
    ns = Namespace [src]
logStr' = Katip.logStr . FastLogger.fromLogStr . Logger.toLogStr
