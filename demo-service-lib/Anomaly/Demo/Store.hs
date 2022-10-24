{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Anomaly.Demo.Store (
  ColorRibbonStore (..),
  lookupColorRibbonOrError,
  deleteColorRibbon,
  insertOrUpdateColorRibbon,
  runStorePure,
  runStoreAsSqlite,
) where

import Anomaly.Demo.Domain
import qualified Anomaly.Demo.SqliteDB as DB
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Database.SQLite.Simple as Sqlite
import Polysemy
import qualified Polysemy.Error as Polysemy
import qualified Polysemy.State as Polysemy

data ColorRibbonStore r a where
  -- | Try looking up a 'ColorRibbonId' corresponding to a given aspect.
  LookupColorRibbon :: AspectName -> ColorRibbonStore r (Maybe ColorRibbonId)
  -- | Delete ribbons associated to a given aspect.
  DeleteColorRibbon :: AspectName -> ColorRibbonStore r ()
  -- | Create a new association, or overwrite an existing ones.
  InsertOrUpdateColorRibbon :: AspectName -> ColorRibbonId -> ColorRibbonStore r UpdateResponse

-- | Try looking up a 'ColorRibbonId' corresponding to a given aspect.
lookupColorRibbon ::
  Member ColorRibbonStore r =>
  AspectName ->
  Sem r (Maybe ColorRibbonId)
lookupColorRibbon aspect = send (LookupColorRibbon aspect)

-- | Delete ribbons associated to a given aspect.
deleteColorRibbon ::
  Member ColorRibbonStore r =>
  AspectName ->
  Sem r ()
deleteColorRibbon aspect = send (DeleteColorRibbon aspect)

-- | Create a new association, or overwrite an existing ones.
insertOrUpdateColorRibbon ::
  Member ColorRibbonStore r =>
  AspectName ->
  ColorRibbonId ->
  Sem r UpdateResponse
insertOrUpdateColorRibbon aspect ribbon = send (InsertOrUpdateColorRibbon aspect ribbon)

lookupColorRibbonOrError ::
  Members '[ColorRibbonStore, Polysemy.Error LookupError] r =>
  AspectName ->
  Sem r ColorRibbonId
lookupColorRibbonOrError aspect =
  maybe (Polysemy.throw (NotFound aspect)) pure =<< lookupColorRibbon aspect

runStoreAsState ::
  Sem (ColorRibbonStore ': r) a ->
  Sem (Polysemy.State (Map AspectName ColorRibbonId) ': r) a
runStoreAsState = reinterpret $ \case
  LookupColorRibbon aspect -> Polysemy.gets $ Map.lookup aspect
  DeleteColorRibbon aspect -> do
    updatedStore <- Polysemy.gets $ Map.delete aspect
    Polysemy.modify $ const updatedStore
  InsertOrUpdateColorRibbon aspect ribbon ->
    let insertLookup = Map.insertLookupWithKey (\_ a _ -> a) aspect ribbon
     in do
          (maybePrevious, updatedStore) <- Polysemy.gets insertLookup
          Polysemy.modify $ const updatedStore
          pure $ maybe ACRLinkCreated (const ACRLinkUpdated) maybePrevious

runStorePure ::
  Map AspectName ColorRibbonId ->
  Sem (ColorRibbonStore ': r) a ->
  Sem r (Map AspectName ColorRibbonId, a)
runStorePure store = Polysemy.runState store . runStoreAsState

runStoreAsSqlite ::
  Member (Embed IO) r =>
  Sqlite.Connection ->
  Sem (ColorRibbonStore ': r) a ->
  Sem r a
runStoreAsSqlite conn = Polysemy.interpret $ \case
  LookupColorRibbon aspect -> Polysemy.embed $ DB.selectRibbon conn aspect
  DeleteColorRibbon aspect -> Polysemy.embed $ DB.deleteRibbon conn aspect
  InsertOrUpdateColorRibbon aspect ribbon -> Polysemy.embed $ DB.insertOrUpdateRibbon conn aspect ribbon
