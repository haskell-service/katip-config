{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module:      Katip.Config
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: DeriveGeneric, LambdaCase, OverloadedStrings, RecordWildCards
--
-- Utilities to easily configure "Katip" based logging without hard-coding
-- settings.
--
-- Basic usage:
--
-- @
-- main :: IO ()
-- main = runEnv' $ do
--     $(logTM) InfoS "Application starting"
-- @
--
-- and run the resulting executable e.g. as
--
-- @
-- \$ LOGGING=stderr:?severity=warning ./katip-config-example
-- @
--
-- See 'runEnv'' and 'configFromEnv' to learn more about the defaults used.
--
-- #configuration-string-format#
--
-- == Configuration String Format
--
-- Configuration strings are URIs, using the /scheme/ to select a handler, and
-- query arguments to set options.
--
-- === Common Options
-- [@severity@] Minimal severity of logged messages, as a lowercase string.
-- Valid options are /debug/, /info/, /notice/, /warning/, /error/, /critical/,
-- /alert/ or /emergency/. The default value is /info/.
--
-- [@verbosity@] Verbosity of logged payloads (see 'Verbosity'), as an numeric
-- value. Valid options are /0/, /1/, /2/ or /3/. The default value is /2/.
--
-- [@buffersize@] Size of the message buffer, as a numeric value. The default
-- value is whatever 'defaultScribeSettings' specifies.
--
-- === Handlers
-- [@stdout:@] Log messages are sent to 'stdout'. A 'ColorStrategy' can be
-- selected using the /color/ option, which can be left out or empty, which
-- selects the default 'ColorIfTerminal' strategy, or /0/ or /1/ to explicitly
-- enable or disable colors.
--
-- [@stderr:@] Similar to /stdout:/, but using 'stderr'.
--
-- [@file:@] Append log messages to a file specified by the /path/ of the URI.
--
-- [@journal:@] Log to the <https://www.freedesktop.org/wiki/Software/systemd/ systemd>
-- <https://www.freedesktop.org/software/systemd/man/systemd-journald.service.html journal service>.
-- An optional 'Facility' can be provided using the /facility/ option, as a
-- string value. Valid options are /kernel/, /user/, /mail/, /news/, /uucp/,
-- /daemon/, /auth/, /cron/, /lpr/ and /local0/ to /local7/. The default is not
-- to specify a facility (see also 'journalScribe').
--
-- === Examples
-- Log to console:
--
-- @
-- stdout:?severity=debug
-- stderr:
-- stderr:?verbosity=0&severity=notice&buffersize=2048
-- @
--
-- Log to a file:
--
-- @
-- file:\/var\/log\/myservice.log
-- @
--
-- Log to a file in the current directory:
--
-- @
-- file:test.log?severity=debug
-- @
--
-- Log to the systemd journal:
--
-- @
-- journal:
-- journal:?facility=mail&severity=info
-- @


module Katip.Config (
    -- * Zero-configuration interface
      runEnv'
    , runEnv
    -- * Lower-level interface
    , Config(..)
    , configFromEnv
    , run
    -- * Plumbing
    , ScribeConfig(..)
    , defaultScribeConfig
    , ScribeDef(..)
    , parseScribeConfig
    , withScribe
    ) where

import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import System.Environment (getProgName, lookupEnv)
import System.IO (Handle, IOMode(AppendMode), hClose, openFile, stderr, stdout)
import Text.Read (readMaybe)

import Control.Monad.IO.Class (MonadIO, liftIO)

import Network.URI (URI(uriPath, uriQuery, uriScheme), parseAbsoluteURI)

import Network.HTTP.Types.URI (parseQuery)

import Control.Exception.Safe (MonadMask, bracket)

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.ByteString.Char8 as BS8

import System.Posix.Syslog (
    Facility(Auth, Cron, Daemon, Kernel,
             Local0, Local1, Local2, Local3, Local4, Local5, Local6, Local7,
             LPR, Mail, News, UUCP, User))

import Katip (
    Environment(Environment), KatipContextT, LogItem, Namespace(Namespace), Scribe, ScribeSettings,
    Severity(AlertS, CriticalS, DebugS, EmergencyS, ErrorS, InfoS, NoticeS, WarningS),
    Verbosity(V0, V1, V2, V3),
    _scribeBufferSize, closeScribes, defaultScribeSettings, initLogEnv,
    registerScribe, runKatipContextT)
import Katip.Scribes.Handle (ColorStrategy(ColorIfTerminal, ColorLog), mkHandleScribe)
import Katip.Scribes.Journal (journalScribe)

-- | Definition of a 'Scribe'.
data ScribeDef = HandleScribe (IO Handle) (Handle -> IO ()) ColorStrategy Severity Verbosity -- ^ A 'Scribe' created using 'mkHandleScribe'
               | JournalScribe (Maybe Facility) Severity Verbosity -- ^ A 'Scribe' created using 'journalScribe'
    deriving (Generic)

-- Note: manual instances because functions aren't Show'able
instance Show ScribeDef where
    showsPrec d = \case
        HandleScribe _ _ c s v -> showParen (d >= 11) ( showString "HandleScribe _ _ "
                                                      . showsColorStrategy 11 c
                                                      . showChar ' '
                                                      . showsPrec 11 s
                                                      . showChar ' '
                                                      . showsPrec 11 v
                                                      )
        JournalScribe f s v -> showParen (d >= 11) ( showString "JournalScribe "
                                                   . showsPrec 11 f
                                                   . showChar ' '
                                                   . showsPrec 11 s
                                                   . showChar ' '
                                                   . showsPrec 11 v
                                                   )
      where
        showsColorStrategy e = \case
            ColorIfTerminal -> showParen (e >= (11 :: Int)) (showString "ColorIfTerminal")
            ColorLog b -> showParen (e >= 11) ( showString "ColorLog "
                                              . showChar ' '
                                              . showsPrec 11 b
                                              )

-- | Configuration of a 'Scribe'.
data ScribeConfig = ScribeConfig ScribeDef ScribeSettings

-- Note: manual instances because ScribeSettings has no Show instance (for now)
instance Show ScribeConfig where
    showsPrec d (ScribeConfig sd ss) = showParen (d >= 11) ( showString "ScribeConfig "
                                                           . showsPrec 11 sd
                                                           . showChar ' '
                                                           . showsScribeSettings 11 ss
                                                           )
      where
        showsScribeSettings e s = showParen (e >= (11 :: Int)) ( showString "ScribeSettings {"
                                                               . showString "_scribeBufferSize = "
                                                               . shows (_scribeBufferSize s)
                                                               . showChar '}'
                                                               )

-- | Default 'ScribeConfig'.
--
-- This configuration logs to 'stderr' using the 'ColorIfTerminal' strategy,
-- 'InfoS' as minimal 'Severity', 'V2' as 'Verbosity', and
-- 'defaultScribeSettings'.
defaultScribeConfig :: ScribeConfig
defaultScribeConfig = ScribeConfig defaultScribeDef defaultScribeSettings

defaultScribeDef :: ScribeDef
defaultScribeDef = HandleScribe (pure stderr) (const $ pure ()) defaultColorStrategy defaultSeverity defaultVerbosity

defaultSeverity :: Severity
defaultSeverity = InfoS

defaultVerbosity :: Verbosity
defaultVerbosity = V2

defaultColorStrategy :: ColorStrategy
defaultColorStrategy = ColorIfTerminal

-- | Parse a configuration 'String' into a 'ScribeConfig', or return an error message.
--
-- See "Katip.Config#configuration-string-format" for more information.
parseScribeConfig :: String -> Either String ScribeConfig
parseScribeConfig cfg = do
    uri <- maybe (err "Invalid URI format") pure (parseAbsoluteURI cfg)

    let query = parseQuery $ BS8.pack $ uriQuery uri -- TODO BS8.pack is icky
    case uriScheme uri of
        "stdout:" -> ScribeConfig <$> parseTerminal stdout query
                                  <*> parseScribeSettings query
        "stderr:" -> ScribeConfig <$> parseTerminal stderr query
                                  <*> parseScribeSettings query
        "file:" -> ScribeConfig <$> parseFile uri query
                                <*> parseScribeSettings query
        "journal:" -> ScribeConfig <$> parseJournal query
                                   <*> parseScribeSettings query
        scheme -> err ("Unknown scheme " ++ show scheme)
  where
    err :: String -> Either String a
    err = Left

    parseTerminal handle query = do
        (severity, verbosity) <- parseCommon query

        colorStrategy <- case lookup "color" query of
            Nothing -> pure defaultColorStrategy
            Just Nothing -> pure defaultColorStrategy
            Just (Just "0") -> pure $ ColorLog False
            Just (Just "1") -> pure $ ColorLog True
            Just (Just v) -> err ("Invalid color strategy " ++ show v)

        pure $ HandleScribe (pure handle)
                            (const $ pure ())
                            colorStrategy
                            (fromMaybe defaultSeverity severity)
                            (fromMaybe defaultVerbosity verbosity)

    parseFile uri query = do
        (severity, verbosity) <- parseCommon query
        path <- case uriPath uri of
            "" -> err "Missing log file path"
            path -> pure path
        pure $ HandleScribe (openFile path AppendMode)
                            hClose
                            (ColorLog False)
                            (fromMaybe defaultSeverity severity)
                            (fromMaybe defaultVerbosity verbosity)

    parseJournal query = do
        (severity, verbosity) <- parseCommon query

        facility <- case lookup "facility" query of
            Nothing -> pure Nothing
            Just Nothing -> err "Missing facility value"
            Just (Just v) -> Just `fmap` case v of
                "kernel" -> pure Kernel
                "user" -> pure User
                "mail" -> pure Mail
                "news" -> pure News
                "uucp" -> pure UUCP
                "daemon" -> pure Daemon
                "auth" -> pure Auth
                "cron" -> pure Cron
                "lpr" -> pure LPR
                "local0" -> pure Local0
                "local1" -> pure Local1
                "local2" -> pure Local2
                "local3" -> pure Local3
                "local4" -> pure Local4
                "local5" -> pure Local5
                "local6" -> pure Local6
                "local7" -> pure Local7
                _ -> err ("Invalid facility " ++ show v)

        pure $ JournalScribe facility
                             (fromMaybe defaultSeverity severity)
                             (fromMaybe defaultVerbosity verbosity)

    parseCommon query = do
        severity <- case lookup "severity" query of
            Nothing -> pure Nothing
            Just Nothing -> err "Missing severity value"
            Just (Just severity) -> Just `fmap` parseSeverity severity
        verbosity <- case lookup "verbosity" query of
            Nothing -> pure Nothing
            Just Nothing -> err "Missing verbosity value"
            Just (Just verbosity) -> Just `fmap` parseVerbosity verbosity

        pure (severity, verbosity)

    parseSeverity = \case
        "debug" -> pure DebugS
        "info" -> pure InfoS
        "notice" -> pure NoticeS
        "warning" -> pure WarningS
        "error" -> pure ErrorS
        "critical" -> pure CriticalS
        "alert" -> pure AlertS
        "emergency" -> pure EmergencyS
        s -> err ("Invalid severity " ++ show s)

    parseVerbosity = \case
        "0" -> pure V0
        "1" -> pure V1
        "2" -> pure V2
        "3" -> pure V3
        s -> err ("Invalid verbosity " ++ show s)

    parseScribeSettings query = do
        let settings = defaultScribeSettings

        case lookup "buffersize" query of
            Nothing -> pure settings
            Just Nothing -> err "Missing buffersize value"
            Just (Just v) -> case readMaybe (BS8.unpack v) of
                Just v' -> pure $ settings { _scribeBufferSize = v' }
                Nothing -> err ("Invalid buffersize " ++ show v)

-- | Use a 'Scribe' defined by a 'ScribeDef'.
withScribe :: ( MonadIO m
              , MonadMask m
              )
           => ScribeDef
           -> (Scribe -> m a)
           -> m a
withScribe scribe action = case scribe of
    HandleScribe open close colorStrategy severity verbosity ->
        bracket (liftIO open)
                (liftIO . close)
                (\h -> liftIO (mkHandleScribe colorStrategy h severity verbosity) >>= action)
    JournalScribe facility severity verbosity -> action $ journalScribe facility severity verbosity

-- | Configuration of a "Katip" envirionment.
data Config c = Config { configScribeConfig :: ScribeConfig
                       , configScribeName :: Text
                       , configBaseNamespace :: Namespace
                       , configEnvironment :: Environment
                       , configInitialContext :: c
                       , configInitialNamespace :: Namespace
                       }
    deriving (Show, Generic)

-- | Retrieve a 'Config' from the process environment.
--
-- This action creates a 'Config' based on environment variables, and some
-- default values:
--
-- 'configScribeConfig' is set to the result of 'parseScribeConfig' on the
-- value of the /LOGGING/ environment variable, if present. Otherwise,
-- 'defaultScribeConfig' is used.
--
-- 'configScribeName' is /"config"/.
--
-- 'configBaseNamespace' is set to the program name (see 'getProgName').
--
-- 'configEnvironment' is set to the value of the /ENV/ environment variable, or
-- /"production"/ if this variable is not set.
--
-- 'configInitialContext' is set to /()/.
--
-- 'configInitialNamespace' is set to /"main"/.
--
-- If 'parseScribeConfig' results in an error, this is returned as-is.
configFromEnv :: IO (Either String (Config ()))
configFromEnv = do
    scribeConfig <- maybe (Right defaultScribeConfig) parseScribeConfig <$> lookupEnv "LOGGING"
    either (pure . Left) (fmap Right . mkConfig) scribeConfig
  where
    mkConfig sc = Config sc "config" <$> (Namespace . (:[]) . T.pack <$> getProgName)
                                     <*> (Environment . T.pack . fromMaybe "production" <$> lookupEnv "ENV")
                                     <*> pure ()
                                     <*> pure "main"

-- | Run a 'KatipContext' action in a context defined by a 'Config'.
run :: ( LogItem c
       , MonadIO m
       , MonadMask m
       )
    => Config c
    -> KatipContextT m a
    -> m a
run Config{..} action = withScribe scribeDef $ \scribe ->
    bracket (liftIO $ registerScribe configScribeName scribe scribeSettings =<< initLogEnv configBaseNamespace configEnvironment)
            (liftIO . closeScribes)
            (\env -> runKatipContextT env configInitialContext configInitialNamespace action)
  where
    ScribeConfig scribeDef scribeSettings = configScribeConfig

-- | Run a 'Katip.KatipContext' action using a configuration retrieved from the
-- process environment. Parse errors are returned,
--
-- See 'configFromEnv' and 'parseScribeConfig'.
runEnv :: ( MonadIO m
          , MonadMask m
          )
       => KatipContextT m a
       -> m (Either String a)
runEnv action = do
    cfg <- liftIO configFromEnv
    either (pure . Left) (fmap Right . flip run action) cfg

-- | Run a 'Katip.KatipContext' action using a configuration retrieved from the
-- process environment, and 'fail' on parse errors.
--
-- See 'configFromEnv' and 'parseScribeConfig'.
runEnv' :: ( MonadIO m
           , MonadMask m
           )
       => KatipContextT m a
       -> m a
runEnv' action = runEnv action >>= either fail pure
