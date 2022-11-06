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

import Data.Aeson
import Data.Text hiding (map, tail)
import Lens.Micro
import Lens.Micro.TH
import Effectful.TH
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared
import Data.ByteString.Lazy hiding (map, tail)
import Lens.Micro.Extras
import qualified Data.ByteString as BS
import System.Random
import Control.Monad
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as T hiding (map)
import qualified Data.ByteString.Char8 as BS.Char8
import Data.Aeson.Types

data Breadcrumbs :: Effect where
  WithSpan :: Text -> Text -> m a -> Breadcrumbs m a
  AddAnnotation :: Text -> Breadcrumbs m ()
  AddTag :: ToJSON b => Maybe b -> Text -> Breadcrumbs m ()

makeEffect ''Breadcrumbs

newtype SpanID = SpanID BS.ByteString deriving newtype (Eq, Ord, Show)
newtype TraceID = TraceID BS.ByteString deriving newtype (Eq, Ord, Show)

data Span = Span
  { _spanName :: Text
  , _spanParent :: Maybe SpanID
  , _spanId :: SpanID
  , _spanTimestamp :: Int
  -- duration and traceId are added later
  , _spanService :: Text
  , _spanAnnotations :: [(Text, Int)]
  , _spanTags :: [(Text, ByteString)]
  }

data BreadcrumbTrail = BreadcrumbTrail
  { _spans :: [Span]
  , _completedSpans :: [BS.ByteString]
  , _rootId :: TraceID
  }

makeLenses ''Span
makeLenses ''BreadcrumbTrail

addJsonKey :: Text -> Value -> Object -> Object
addJsonKey key = KM.insert (fromText key)

runBreadcrumbsToFile ::
  IOE :> es
  => Eff (Breadcrumbs ': es) a
  -> Eff es a
runBreadcrumbsToFile = reinterpret (evalState (BreadcrumbTrail [] [] (TraceID ""))) $ \env -> \case
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

getCPUTime :: Eff (State BreadcrumbTrail : es) b0
getCPUTime = error ""

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) f = f x
whenJust Nothing _ = pure ()

popSpan :: Eff (State BreadcrumbTrail : es) ()
popSpan = do
  (sp :: Maybe Span) <- gets $ preview (spans . ix 0)
  modify (spans %~ tail)
  whenJust sp finaliseSpan

finaliseSpan :: Span -> Eff (State BreadcrumbTrail : es) ()
finaliseSpan sp = do
  now <- getCPUTime
  tId <- gets _rootId
  let v = encode $ object $
        [ "traceId" .= encodeTraceID tId
        , "name" .= _spanName sp
        , "timestamp" .= _spanTimestamp sp
        , "duration" .= (now - _spanTimestamp sp)
        , "localEndpoint" .= object
          [ "serviceName" .= _spanService sp
          ]
        , "annotations" .= listValue (\(n, ts) -> object
            [ "timestamp" .= ts
            , "value" .= n
            ]) (_spanAnnotations sp)
        , "tags" .= object (map (\(n, bs) -> fromText n .= bs) (_spanTags sp))
        ]
        ++
        maybe [] (\s -> ["parentId" .= encodeSpanID s]) (_spanParent sp)
  pure ()

addSpan :: IOE :> es => Text -> Text -> Eff (State BreadcrumbTrail : es) ()
addSpan serviceName sName = do
  now <- getCPUTime
  sId <- liftIO $ randomSpanID
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
  pure ()

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
decodeSpanID :: Text -> Maybe SpanID
decodeSpanID txt = case hexDecode txt of
  Just bs | BS.length bs == 8 -> Just $ SpanID bs
  _ -> Nothing