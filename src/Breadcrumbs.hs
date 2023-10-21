{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Breadcrumbs
  ( module Breadcrumbs
  , SpanID(..)
  , TraceID(..)
  , Span(..)

  ) where

import Data.List (findIndex)
import Data.Text ( Text )
import Data.Text.Display
import Effectful ( type (:>), Effect, MonadIO(liftIO), Eff, IOE )
import Effectful.Dispatch.Dynamic ( localSeqUnlift, reinterpret, HasCallStack )
import Effectful.State.Static.Shared ( State, evalState, gets, modify )
import Effectful.TH ( makeEffect )
import Lens.Micro ( ix, (%~), (.~) )
import Lens.Micro.Extras ( preview )
import Breadcrumbs.Internal

type ServiceName = Text
type Name = Text
-- | The logging effect.
data Breadcrumbs :: Effect where
  -- | Wrap a block with a `Span`.
  WithSpan :: ServiceName -> Name -> (SpanID -> m a) -> Breadcrumbs m a
  -- | Add a text annotation to a specific `Span` (`Just`) in the breadcrumb trail or to the current `Span` (`Nothing`).
  AddAnnotationTo :: Maybe SpanID -> Text -> Breadcrumbs m ()
  -- | Add a tag to a specific `Span` (`Just`) in the breadcrumb trail or to the current `Span` (`Nothing`).
  AddTagTo :: Display a => Maybe SpanID -> Text -> a -> Breadcrumbs m ()
  -- | Retriece the current `Span`, if one exists.
  GetCurrentSpan :: Breadcrumbs m (Maybe Span)
  -- | Modify the current `Span`.
  ModifySpan :: (Span -> Span) -> Breadcrumbs m ()
  -- | Output the currently finished spans to the given output channel.
  Flush :: Breadcrumbs m ()
  -- | Throw away the current `Span`. Useful if you want to automate a new span in some situation, but throw away the
  -- trivial cases to avoid noise (e.g. empty `Span`s.)
  IgnoreSpan :: Breadcrumbs m ()
  -- | Retrieve the current root `TraceID`. This always exists.
  GetTraceId :: Breadcrumbs m TraceID
  -- | Wrap a block without a `Span`.
  WithoutSpan :: m a -> Breadcrumbs m a




makeEffect ''Breadcrumbs

-- | Wrap a block with a `Span`, but ignore the `SpanID`. This is the most commonly used form of `withSpan`;
-- the more general `withSpan` is useful if you wish to squash two `Span`s into one by using a parent span
-- instead of creating a new span.
withSpan' ::
  Breadcrumbs :> es
  => ServiceName
  -> Name
  -> Eff es a
  -> Eff es a
withSpan' sService sName inner = withSpan sService sName (const inner)

-- | Interpret the `Breadcrumbs` effect.
runBreadcrumbs ::
  IOE :> es
  => Maybe TraceID
  -> Eff (Breadcrumbs ': es) a
  -> Eff es a
runBreadcrumbs mbId = reinterpret (\e -> do
    rootId' <- maybe (liftIO randomTraceID) pure mbId
    evalState (BreadcrumbTrail [] [] rootId' True) e ) $ \env -> \case
  WithSpan sService sName inner -> do
    e <- gets _enableSpans
    if e then
      do
        i <- addSpan sService sName
        a <- localSeqUnlift env $ \unlift -> unlift (inner i)
        popSpan (Just i)
        pure a
      else do
        mbSpan <- gets $ preview (spans . ix 0)
        maybe (error "cannot ignore a span if we have no span")
          (\s -> localSeqUnlift env $ \unlift -> unlift (inner (_spanId s))) mbSpan

  AddAnnotationTo mbSpanId anno -> do
    i <- maybe (pure 0) getIndexFor mbSpanId
    ts <- getCPUTime
    modify (spans . ix i . spanAnnotations %~ ((anno, ts):))
  AddTagTo mbSpanId tagName metadata -> do
    i <- maybe (pure 0) getIndexFor mbSpanId
    modify (spans . ix i . spanTags %~ ((tagName, display metadata):))
  GetCurrentSpan -> do
    gets $ preview (spans . ix 0)
  GetTraceId -> gets _rootId
  Flush -> pushToZipkin
  IgnoreSpan -> popSpan Nothing
  ModifySpan f -> modify (spans . ix 0 %~ f)
  WithoutSpan inner -> do
    eS <- gets _enableSpans
    modify (enableSpans .~ False)
    a <- localSeqUnlift env $ \unlift -> unlift inner
    modify (enableSpans .~ eS)
    pure a

-- | Add an annotation to a specific `Span`.
addAnnotationToSpan :: HasCallStack => Breadcrumbs :> es => SpanID -> Text -> Eff es ()
addAnnotationToSpan sId = addAnnotationTo (Just sId)

-- | Add an annotation to the current `Span`.
addAnnotation :: HasCallStack => Breadcrumbs :> es => Text -> Eff es ()
addAnnotation = addAnnotationTo Nothing

  -- | Add a tag to a specific `Span` (`Just`) in the breadcrumb trail.
addTagToSpan :: HasCallStack => Breadcrumbs :> es => SpanID -> Text -> Text -> Eff es ()
addTagToSpan sId = addTagTo (Just sId)

  -- | Add a tag to the current `Span`.
addTag :: HasCallStack => Display a => Breadcrumbs :> es => Text -> a -> Eff es ()
addTag = addTagTo Nothing

getIndexFor :: HasCallStack => SpanID -> Eff (State BreadcrumbTrail : es) Int
getIndexFor sId = do
  s <- gets _spans
  case findIndex (\Span{..} -> _spanId == sId) s of
    Nothing -> error $ "Could not find index " <> show sId
    Just x -> pure x
