{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Breadcrumbs.Internal where

import Control.Exception (catch, SomeException)
import Control.Monad ( replicateM, void, when )
import Data.Aeson ( encode )
import Data.Aeson.Key (fromText)
import Data.Aeson.Types
import Data.Text ( Text )
import Data.Time
import Data.Time.Clock.POSIX ( POSIXTime, getPOSIXTime )
import Effectful ( type (:>), MonadIO(liftIO), Eff, IOE )
import Effectful.State.Static.Shared ( State, gets, modify )
import Lens.Micro
import Lens.Micro.Extras (preview)
import Lens.Micro.TH ( makeLenses )
import System.Random ( randomIO )
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.Text as T hiding (map)
import qualified Network.HTTP.Client as HTTP

newtype SpanID = SpanID BS.ByteString deriving newtype (Eq, Ord, Show)
newtype TraceID = TraceID BS.ByteString deriving newtype (Eq, Ord, Show)

randomID :: Int -> IO BS.ByteString
randomID len = BS.pack <$> replicateM len randomIO

-- | Generates a random trace ID.
randomTraceID :: IO TraceID
randomTraceID = TraceID <$> randomID 16

-- | Generates a random span ID.
randomSpanID :: IO SpanID
randomSpanID = SpanID <$> randomID 8

hexDecode :: Text-> Maybe BS.ByteString
hexDecode t = case Base16.decode $ BS.Char8.pack $ T.unpack t of
  Right bs -> Just bs
  _ -> Nothing

hexEncode :: BS.ByteString -> Text
hexEncode = T.pack . BS.Char8.unpack . Base16.encode

-- | Hex-encodes a span ID.
encodeSpanID :: SpanID -> Text
encodeSpanID (SpanID bs) = hexEncode bs

encodeTraceID :: TraceID -> Text
encodeTraceID (TraceID bs) = hexEncode bs

-- | Decodes a span ID from a hex-encoded string.
decodeTraceID :: Text -> Maybe TraceID
decodeTraceID txt = case hexDecode txt of
  Just bs | BS.length bs == 16 -> Just $ TraceID bs
  _ -> Nothing

-- | Decodes a span ID from a hex-encoded string.
decodeSpanID :: Text -> Maybe SpanID
decodeSpanID txt = case hexDecode txt of
  Just bs | BS.length bs == 8 -> Just $ SpanID bs
  _ -> Nothing

pushToZipkin :: IOE :> es => Eff (State BreadcrumbTrail : es) ()
pushToZipkin = do
  spns <- gets _completedSpans
  let req = HTTP.defaultRequest
        { HTTP.method = "POST"
        , HTTP.host = BS.pack "localhost"
        , HTTP.path = "/api/v2/spans"
        , HTTP.port = 9411
        , HTTP.requestHeaders = [("content-type", "application/json")]
        }
  let req' = req { HTTP.requestBody = HTTP.RequestBodyLBS $ encode spns }
  mgr <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  void $ liftIO (void (HTTP.httpLbs req' mgr) `catch` (\(_a :: SomeException) -> pure ()))

data Span = Span
  { _spanName :: Text
  , _spanParent :: Maybe SpanID
  , _spanId :: SpanID
  , _spanTimestamp :: POSIXTime
  , _spanService :: Text
  , _spanAnnotations :: [(Text, POSIXTime)]
  , _spanTags :: [(Text, Text)]
  } deriving stock (Show)

data BreadcrumbTrail = BreadcrumbTrail
  { _spans :: [Span]
  , _completedSpans :: [Value]
  , _rootId :: TraceID
  , _enableSpans :: Bool
  }

makeLenses ''Span
makeLenses ''BreadcrumbTrail

getCPUTime :: IOE :> es => Eff (State BreadcrumbTrail : es) NominalDiffTime
getCPUTime = liftIO getPOSIXTime

microSeconds :: NominalDiffTime -> Int
microSeconds = round . (* 1000000)

popSpan :: IOE :> es => Maybe SpanID -> Eff (State BreadcrumbTrail : es) ()
popSpan mbSpanToPop = do
  (sp :: Maybe Span) <- gets $ preview (spans . ix 0)
  case sp of
    Nothing -> pure ()
    Just sp' -> when (maybe True (_spanId sp' ==) mbSpanToPop) $ do
      modify (spans %~ tail)
      case mbSpanToPop of
        Nothing -> pure () -- discard
        Just _ -> finaliseSpan sp'

finaliseSpan :: IOE :> es => Span -> Eff (State BreadcrumbTrail : es) ()
finaliseSpan sp = do
  now <- getCPUTime
  tId <- gets _rootId
  let v = object $
        [ "traceId" .= encodeTraceID tId
        , "name" .= _spanName sp
        , "id" .= encodeSpanID (_spanId sp)
        , "timestamp" .= microSeconds (_spanTimestamp sp)
        , "duration" .= microSeconds (now - _spanTimestamp sp)
        , "kind" .= ("CLIENT" :: String)
        , "localEndpoint" .= object
          [ "serviceName" .= _spanService sp
          ]
        , "annotations" .= listValue (\(n, ts) -> object
            [ "timestamp" .= microSeconds ts
            , "value" .= n
            ]) (_spanAnnotations sp)
        , "tags" .= object (map (\(n, bs) -> fromText n .= bs) (_spanTags sp))
        ]
        ++
        maybe [] (\s -> ["parentId" .= encodeSpanID s]) (_spanParent sp)
  modify (completedSpans %~ (v:))

addSpan :: IOE :> es => Text -> Text -> Eff (State BreadcrumbTrail : es) SpanID
addSpan serviceName sName = do
  now <- getCPUTime
  sId <- liftIO randomSpanID
  (sp :: Maybe Span) <- gets $ preview (spans . ix 0)
  let s = Span
        { _spanName = sName
        , _spanParent = _spanId <$> sp
        , _spanId = sId
        , _spanTimestamp = now
        , _spanService = serviceName
        , _spanAnnotations = []
        , _spanTags = []
        }
  modify (spans %~ (s:))
  pure sId