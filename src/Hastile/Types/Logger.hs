{-# LANGUAGE OverloadedStrings #-}
module Hastile.Types.Logger where

import qualified Control.Monad.Logger  as Logger
import qualified Katip
import qualified System.IO             as IO
import qualified System.Log.FastLogger as FastLogger

fromLevel :: Logger.LogLevel -> Katip.Severity
fromLevel Logger.LevelDebug     = Katip.DebugS
fromLevel Logger.LevelInfo      = Katip.InfoS
fromLevel Logger.LevelWarn      = Katip.WarningS
fromLevel Logger.LevelError     = Katip.ErrorS
fromLevel (Logger.LevelOther _) = Katip.NoticeS

defaultLogEnv :: Katip.Environment -> IO Katip.LogEnv
defaultLogEnv environment = do
    handleScribe <- Katip.mkHandleScribe Katip.ColorIfTerminal IO.stdout (Katip.permitItem Katip.DebugS) Katip.V2
    env <- Katip.initLogEnv "hastile" environment
    Katip.registerScribe "stdout" handleScribe Katip.defaultScribeSettings env

adapt :: (FastLogger.ToLogStr msg, Applicative m, Katip.Katip m) => (Katip.Namespace -> Katip.Severity -> Katip.LogStr -> m ()) -> Logger.Loc -> Logger.LogSource -> Logger.LogLevel -> msg -> m ()
adapt f _ src lvl msg =
    f ns (fromLevel lvl) $ logStr' msg
  where
    ns = Katip.Namespace [src]

logStr' :: (FastLogger.ToLogStr msg) => msg -> Katip.LogStr
logStr' = Katip.logStr . FastLogger.fromLogStr . Logger.toLogStr

