{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Breadcrumbs where

import Control.Monad ( replicateM, forM_, void )
import Data.Aeson
import Data.Aeson.Key ( fromText )
import Data.Aeson.Types ( listValue )
import Data.ByteString.Lazy ( ByteString )
import Data.Text ( Text )
import Data.Time ( NominalDiffTime )
import Data.Time.Clock.POSIX ( POSIXTime, getPOSIXTime )
import Effectful ( type (:>), Effect, MonadIO(liftIO), Eff, IOE )
import Effectful.Dispatch.Dynamic ( localSeqUnlift, reinterpret )
import Effectful.State.Static.Shared
    ( State, evalState, gets, modify )
import Effectful.TH ( makeEffect )
import Lens.Micro ( ix, (%~) )
import Lens.Micro.Extras ( preview )
import Lens.Micro.TH ( makeLenses )
import System.Random ( randomIO )
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.Text as T hiding (map)
import qualified Network.HTTP.Client as HTTP
import Control.Exception (catch, SomeException)

data Breadcrumbs :: Effect where
  WithSpan :: Text -> Text -> m a -> Breadcrumbs m a
  AddAnnotation :: Text -> Breadcrumbs m ()
  AddTag :: ToJSON b => Maybe b -> Text -> Breadcrumbs m ()
  GetCurrentSpan :: Breadcrumbs m (Maybe Span)
  Flush :: Breadcrumbs m ()
  GetTraceId :: Breadcrumbs m TraceID

newtype SpanID = SpanID BS.ByteString deriving newtype (Eq, Ord, Show)
newtype TraceID = TraceID BS.ByteString deriving newtype (Eq, Ord, Show)

data Span = Span
  { _spanName :: Text
  , _spanParent :: Maybe SpanID
  , _spanId :: SpanID
  , _spanTimestamp :: POSIXTime
  , _spanService :: Text
  , _spanAnnotations :: [(Text, POSIXTime)]
  , _spanTags :: [(Text, ByteString)]
  } deriving stock (Show)

data BreadcrumbTrail = BreadcrumbTrail
  { _spans :: [Span]
  , _completedSpans :: [Value]
  , _rootId :: TraceID
  }

makeEffect ''Breadcrumbs
makeLenses ''Span
makeLenses ''BreadcrumbTrail

runBreadcrumbs ::
  IOE :> es
  => Maybe TraceID
  -> Eff (Breadcrumbs ': es) a
  -> Eff es a
runBreadcrumbs mbId = reinterpret (\e -> do
    rootId' <- maybe (liftIO randomTraceID) pure mbId
    evalState (BreadcrumbTrail [] [] rootId') e ) $ \env -> \case
  WithSpan sService sName inner -> do
    addSpan sService sName
    a <- localSeqUnlift env $ \unlift -> unlift inner
    popSpan
    pure a
  AddAnnotation anno -> do
    ts <- getCPUTime
    modify (spans . ix 0 . spanAnnotations %~ ((anno, ts):))
  AddTag mbMetadata tagName -> do
    modify (spans . ix 0 . spanTags %~ ((tagName, maybe "" encode mbMetadata):))
  GetCurrentSpan -> do
    gets $ preview (spans . ix 0)
  GetTraceId -> gets _rootId
  Flush -> pushToZipkin

getCPUTime :: IOE :> es => Eff (State BreadcrumbTrail : es) NominalDiffTime
getCPUTime = liftIO getPOSIXTime

microSeconds :: NominalDiffTime -> Int
microSeconds = round . (* 1000000)

popSpan :: IOE :> es => Eff (State BreadcrumbTrail : es) ()
popSpan = do
  (sp :: Maybe Span) <- gets $ preview (spans . ix 0)
  modify (spans %~ tail)
  forM_ sp finaliseSpan

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
        , "tags" .= object (map (\(n, bs) -> fromText n .= decode @Value bs) (_spanTags sp))
        ]
        ++
        maybe [] (\s -> ["parentId" .= encodeSpanID s]) (_spanParent sp)
  modify (completedSpans %~ (v:))

addSpan :: IOE :> es => Text -> Text -> Eff (State BreadcrumbTrail : es) ()
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