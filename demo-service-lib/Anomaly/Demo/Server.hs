{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Anomaly.Demo.Server (
  Config (..),
  runServer,
) where

import Anomaly.Demo.Domain
import Anomaly.Demo.Server.API
import Anomaly.Demo.Store (ColorRibbonStore (..))
import qualified Anomaly.Demo.Store as Store
import Control.Monad.Except (ExceptT (..))
import Data.Function ((&))
import qualified Database.SQLite.Simple as Sqlite
import qualified Network.Wai.Handler.Warp as Warp
import Polysemy (Embed, Members, Sem)
import qualified Polysemy
import qualified Polysemy.Error as Polysemy
import Servant

-- | Configuration for the things used in this service (servant
-- server, database, ...).
data Config = Config
  { -- | The port the server should run on.
    port :: Int
  , -- | Location of the SQLite database file (by default a relative path)
    dbfile :: FilePath
  }
  deriving stock (Eq, Show)

-- | The handlers for the routes defined in the API.
server ::
  Members
    [ ColorRibbonStore
    , Polysemy.Error LookupError
    , Polysemy.Error ServerError
    ]
    r =>
  ServerT Routes (Sem r)
server =
  fmap API <$> Store.selectAllAspects
  :<|> \a ->
      let aspect = unAPI a
       in API <$> Store.lookupColorRibbonOrError aspect
            :<|> deleteNoContent aspect
            :<|> putNoContent aspect . unAPI
  where
    deleteNoContent aspect = do
      Store.deleteColorRibbon aspect
      return NoContent

    putNoContent ::
      Members
        '[ ColorRibbonStore
         , Polysemy.Error ServerError
         ]
        r =>
      AspectName ->
      ColorRibbonId ->
      Sem r (Union '[WithStatus 201 NoContent, WithStatus 204 NoContent])
    putNoContent aspect ribbon = do
      response <- Store.insertOrUpdateColorRibbon aspect ribbon
      case response of
        ACRLinkCreated -> respond (WithStatus @201 NoContent)
        ACRLinkUpdated -> respond (WithStatus @204 NoContent)

transformToHandler ::
  Config ->
  Sem
    '[ ColorRibbonStore
     , Polysemy.Error LookupError
     , Polysemy.Error ServerError
     , Embed IO
     ]
    a ->
  Handler a
transformToHandler Config{..} sem =
  Sqlite.withConnection dbfile (run sem)
    & liftIntoHandler
  where
    run request conn =
      request
        & Store.runStoreAsSqlite conn
        & Polysemy.mapError toServerError
        & Polysemy.runError @ServerError
        & Polysemy.runM

    liftIntoHandler = Handler . ExceptT
    toServerError = \case
      NotFound _ -> Servant.err404

-- | Build a wai application from our configuration.
mkWaiApplication :: Config -> Application
mkWaiApplication config =
  serve (Proxy @Routes) server'
  where
    server' = hoistServer (Proxy @Routes) (transformToHandler config) server

-- | Run the server with a specific configuration.
runServer :: Config -> IO ()
runServer config@Config{..} = do
  putStrLn $ "Starting demo service on port " <> show port
  Warp.run port (mkWaiApplication config)
