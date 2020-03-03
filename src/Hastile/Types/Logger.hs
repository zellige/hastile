{-# LANGUAGE OverloadedStrings #-}
module Hastile.Types.Logger where

import qualified Control.Monad.IO.Class    as MonadIO
import qualified Control.Monad.Logger      as Logger
import qualified Data.ByteString           as ByteString
import qualified Data.Foldable             as Foldable
import qualified Data.Maybe                as Maybe
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as TextEncoding
import qualified Data.Time                 as Time
import qualified Katip
import qualified Network.HTTP.Types        as HttpTypes
import qualified Network.HTTP.Types.Header as HttpTypesHeaders
import qualified Network.Wai               as Wai
import qualified Network.Wai.Header        as WaiHeader
import qualified System.IO                 as IO
import qualified System.Log.FastLogger     as FastLogger

import qualified Hastile.Types.Time        as Time

fromLevel :: Logger.LogLevel -> Katip.Severity
fromLevel Logger.LevelDebug     = Katip.DebugS
fromLevel Logger.LevelInfo      = Katip.InfoS
fromLevel Logger.LevelWarn      = Katip.WarningS
fromLevel Logger.LevelError     = Katip.ErrorS
fromLevel (Logger.LevelOther _) = Katip.NoticeS

logHandler :: Text.Text -> Katip.Environment -> IO Katip.LogEnv
logHandler "stdout" = stdOutLogEnv
logHandler "stderr" = stdErrLogEnv
logHandler fileName = fileLogEnv (Text.unpack fileName)

stdOutLogEnv :: Katip.Environment -> IO Katip.LogEnv
stdOutLogEnv environment = do
    handleScribe <- Katip.mkHandleScribe Katip.ColorIfTerminal IO.stdout (Katip.permitItem Katip.DebugS) Katip.V2
    env <- Katip.initLogEnv "hastile-stdout" environment
    Katip.registerScribe "stdout" handleScribe Katip.defaultScribeSettings env

stdErrLogEnv :: Katip.Environment -> IO Katip.LogEnv
stdErrLogEnv environment = do
    handleScribe <- Katip.mkHandleScribe Katip.ColorIfTerminal IO.stderr (Katip.permitItem Katip.DebugS) Katip.V2
    env <- Katip.initLogEnv "hastile-stderr" environment
    Katip.registerScribe "stderr" handleScribe Katip.defaultScribeSettings env

fileLogEnv :: IO.FilePath -> Katip.Environment -> IO Katip.LogEnv
fileLogEnv filePath environment = do
    handleScribe <- Katip.mkFileScribe filePath (Katip.permitItem Katip.DebugS) Katip.V2
    env <- Katip.initLogEnv "hastile-file-log" environment
    Katip.registerScribe "access" handleScribe Katip.defaultScribeSettings env

adapt :: (FastLogger.ToLogStr msg, Applicative m, Katip.Katip m) => (Katip.Namespace -> Katip.Severity -> Katip.LogStr -> m ()) -> Logger.Loc -> Logger.LogSource -> Logger.LogLevel -> msg -> m ()
adapt f _ src lvl msg =
    f ns (fromLevel lvl) $ logStr' msg
  where
    ns = Katip.Namespace [src]

logStr' :: (FastLogger.ToLogStr msg) => msg -> Katip.LogStr
logStr' = Katip.logStr . FastLogger.fromLogStr . Logger.toLogStr

apacheLog :: MonadIO.MonadIO m => Katip.LogEnv -> Time.UTCTime -> Wai.Request -> Wai.Response -> m ()
apacheLog env currentTime req res = do
  let responseHeaders = Wai.responseHeaders res
      requestHeaders = Wai.requestHeaders req
      sourceIp = show $ TextEncoding.decodeUtf8 $ Maybe.fromMaybe "" (getSource requestHeaders)
      formattedTime = show $ Time.toText currentTime
      requestMethod = show $ Wai.requestMethod req
      path = show $ Wai.rawPathInfo req <> Wai.rawQueryString req
      httpVersion = show (Wai.httpVersion req)
      status = show . HttpTypes.statusCode $ Wai.responseStatus res
      contentLength = Maybe.maybe "-" show (WaiHeader.contentLength responseHeaders)
      mr = Maybe.maybe "-" show (Wai.requestHeaderReferer req)
      mua = Maybe.maybe "-" show (Wai.requestHeaderUserAgent req)
      finalStr =
        sourceIp    <> " - - ["    <> formattedTime <> "] " <> requestMethod <> " " <> path <> " " <>
        httpVersion <> " "         <> status        <> " "  <> contentLength <> " " <> mr   <> " " <>
        mua
  Katip.runKatipT env $
    Katip.logMsg "web" Katip.InfoS (Katip.logStr finalStr)

getSource :: HttpTypesHeaders.RequestHeaders -> Maybe ByteString.ByteString
getSource hdrs = fmap snd maddr
  where
    maddr = Foldable.find (\x -> fst x `elem` ["x-real-ip", "x-forwarded-for"]) hdrs

