{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs, FlexibleContexts, PolyKinds, TypeOperators, DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies, DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

module StoreServer where

import Control.Monad.Except (ExceptT (..))
import qualified Data.Aeson as Aeson
import Data.Function ((&))
import qualified Database.SQLite.Simple as Sqlite
import qualified Database.SQLite.Simple.FromField as Sqlite
import qualified Database.SQLite.Simple.ToField as Sqlite
import qualified Network.Wai.Handler.Warp as Warp
import Polysemy (Embed, Member, Members, Sem, embed)
import qualified Polysemy
import qualified Polysemy.Error as Polysemy
import Polysemy.Internal (send)
import Servant

newtype Key = Key {unKey :: String}
  deriving stock (Eq, Show)
  deriving stock (Ord)
  deriving (Sqlite.FromField, Sqlite.ToField) via String
  deriving (Servant.FromHttpApiData) via String

newtype Value = Value {unValue :: Int}
  deriving stock (Eq, Show)
  deriving (Sqlite.FromField, Sqlite.ToField) via Int
  deriving (Aeson.ToJSON, Aeson.FromJSON, Servant.FromHttpApiData) via Int

data LookupError = NotFound
  deriving stock (Eq, Show)

data Store m a where
    InsertValue :: Key -> Value -> Store m ()
    LookupKey :: Key -> Store m (Maybe Value)

insertValue :: Member Store r => Key -> Value -> Sem r ()
insertValue key value = send (InsertValue key value)

lookupKey :: Member Store r => Key -> Sem r (Maybe Value)
lookupKey key = send (LookupKey key)

lookupKeyOrError ::
  Members
    '[ Store
     , Polysemy.Error LookupError
     ]
    r =>
  Key ->
  Sem r Value
lookupKeyOrError key =
    maybe (Polysemy.throw NotFound) pure =<< lookupKey key

replaceValue :: Sqlite.Connection -> Key -> Value -> IO ()
replaceValue conn key value =
    Sqlite.execute conn "replace into test (key, value) values (?,?)" (key, value)

selectKey :: Sqlite.Connection -> Key -> IO (Maybe Value)
selectKey conn key = do
    res <- Sqlite.query conn "select value from test where key = ?" (Sqlite.Only key)
    case res of
      (Sqlite.Only result) : _ -> pure $ Just result
      [] -> pure Nothing

runStoreAsSqlite ::
  Member (Embed IO) r =>
  Sqlite.Connection ->
  Sem (Store ': r) a ->
  Sem r a
runStoreAsSqlite conn =
    Polysemy.interpret $ \case
        InsertValue key value -> embed $ replaceValue conn key value
        LookupKey key -> embed $ selectKey conn key

createDbTable :: Sqlite.Connection -> IO ()
createDbTable conn =
  let statement = "create table if not exists test (key text primary key not null, value integer)"
   in Sqlite.execute_ conn statement


{- Access the Store through a REST API
  For more details on the library we use here,
  i.e. Servant, see the servant tutorial:
  
  https://docs.servant.dev/en/stable/tutorial/index.html
 -}

-- Our web API as a type.

type LookupRoute = Capture "key" Key :> Get '[JSON] Value

type InsertRoute = Capture "key" Key :> ReqBody '[JSON] Value :> PutNoContent

type Routes = LookupRoute :<|> InsertRoute

-- Serving our API.
server ::
  Members
    '[ Store
     , Polysemy.Error LookupError
     , Polysemy.Error ServerError
     ]
    r =>
  ServerT Routes (Sem r)
server = lookupKeyOrError :<|> putNoContent
  where
    putNoContent key value = insertValue key value >> pure NoContent

-- Transform computations in "our" Sem monad into computations
-- running in Servant's Handler monad.
transformToHandler ::
  FilePath ->
  Sem
    '[ Store
     , Polysemy.Error LookupError
     , Polysemy.Error ServerError
     , Embed IO
     ]
    a ->
  Handler a
transformToHandler dbfile sem =
  Sqlite.withConnection dbfile (run sem)
    & liftIntoHandler
  where
    run request conn =
      request
        & runStoreAsSqlite conn
        & Polysemy.mapError toServerError
        & Polysemy.runError @ServerError
        & Polysemy.runM

    liftIntoHandler = Handler . ExceptT
    toServerError = \case
      NotFound -> Servant.err404

-- Running the server (in IO)
mkWaiApplication :: FilePath -> Application
mkWaiApplication db = serve (Proxy @Routes) server'
  where
    server' = hoistServer (Proxy @Routes) (transformToHandler db) server

-- Our server's "main" function
runServer :: FilePath -> IO ()
runServer db = do
  -- First make our evironment is set up properly
  -- Here it means: make sure the database table we use actually exists.
  putStrLn $ "Initializing the database " <> show db
  Sqlite.withConnection db createDbTable
  -- Then start the server listening at port 8999 and serving our web API.
  let port = 8999
  putStrLn $ "Starting demo server at port " <> show port
  Warp.run port (mkWaiApplication db)
