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

{- the following functions are pure boilerplate code
   and could be generated with template haskell

   one reason we decided against that in our code base
   is the fact that this would hide certain facts;
   the existence of functions named insertValue and
   lookupKey for example. you would have to just know
   that and could not infer it from the code alone.
 -}

insertValue :: Member Store r => Key -> Value -> Sem r ()
insertValue key value = send (InsertValue key value)

lookupKey :: Member Store r => Key -> Sem r (Maybe Value)
lookupKey key = send (LookupKey key)

-- making it more convenient to lookup keys (that might not exist)
lookupKeyOrError ::
  Members
    '[ Store
     , Polysemy.Error LookupError -- gives us Polysemy.throw
     ]
    r =>
  Key ->
  Sem r Value
lookupKeyOrError key =
    -- throw a (domain) error if the key does not exist
    -- otherwise return the Value (unwrapping Just)
    maybe (Polysemy.throw NotFound) pure =<< lookupKey key
    -- the execution is short-circuited, i.e. exited early,
    -- if lookupKey yields Nothing
    --
    -- with this a caller would only ever see Value and does
    -- not have to deal with Maybe Value cases like with the
    -- implementation in BiancaLib (see makeJohannesYounger)

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

type Routes =
  -- GET /:key/ ---> LookupKey
  -- response: Value
  -- error response in case HTTP Not Found (404)
  Capture "key" Key :> Get '[JSON] Value
  -- PUT /:key/:value/ ---> InsertValue
  -- response: HTTP No Content (204)
  :<|> Capture "key" Key :> Capture "value" Value :> PutNoContent

{- Some combinators Servant defines:

  -- URL parameter, e.g. /:key/
  --      tag (*)   (haskell) type
  Capture "key"     Key
  -- (*): tags are used to distinguish multiple segments with the same type
  -- Capture translates into an input parameter of the corresponding handler

  -- Request body, i.e. expected payload
  -- Usually for more complex types that you cannot pass as URL segments directly
  --      Content-Type(s)   (haskell) type
  ReqBody '[JSON]           Value
  -- ReqBody translates into an input parameter of the corresponding handler

  -- Response type
  -- HTTP Verb  Content-Type(s)     (haskell) type
     Get        '[JSON, PlainText]  Value
  -- There are other combinators for the various HTTP verbs, e.g. Put, Post.
  -- There are also special forms for some Verbs:
  -- e.g. 'PutNoContent' means PUT request responding with HTTP response code 204
-}

{- you can group routes with a common prefix: -}
type Routes' =
  Capture "key" Key
    :> ( Get '[JSON] Value
          :<|> Capture "value" Value :> ReqBody '[JSON] Value :> PutNoContent
       )

{- You can split your API into "sub APIs" -}

type LookupRoute =
  -- GET localhost:8080/:key/
  Capture "key" Key :> Get   '[JSON]        Value

type InsertRoute =
  -- PUT localhost:8080/:key/:value/
  -- payload: none
  Capture "key" Key :> Capture "value" Value :> PutNoContent

{- ... and combine these "sub APIs" into more complex APIs: -}
type Routes'' = LookupRoute :<|> InsertRoute -- equivalent to Routes


-- supplying the value for our insert route
-- using request body instead of a URL parameter/segment:
type InsertRoute' =
  -- PUT localhost:8080/:key/
  -- payload: JSON encoded Value
  Capture "key" Key :> ReqBody '[JSON] Value :> PutNoContent
-- Note: InsertRoute and InsertRoute' result in the same
--       type signature that a handler would need:
--          handlerInsert :: Key -> Value -> Sem r NoContent


-- Serving our API.
server ::
  Members
    '[ Store
     , Polysemy.Error LookupError
     , Polysemy.Error ServerError
     ]
    r =>
  ServerT Routes (Sem r)
server =
  -- here our handler functions are bound to different parts of the API
  lookupKeyOrError  -- sometime you can use some effect based computation directly
  :<|> putNoContent -- sometimes you need to tweak it a bit to return the expected type
  -- Note: the structure of the server exactly matches the API definition
  --       You even use the same combinators

  -- grouping make a difference:
  -- if our server was of Type ServerT Routes' (Sem r), we'd need:
  -- \ key -> (lookupKeyOrError key :<|> putNoContent key)

  where
    putNoContent :: Member Store r => Key -> Value -> Sem r NoContent
    putNoContent key value =
      insertValue key value -- call the InsertValue "method" of our Store "interface"
        >> pure NoContent   -- ... and ignoring its result, as it is unit (think: void) anyway
                            --     return the Servant value NoContent, i.e. response code 204 without payload

{- Everything that follows you usually have to figure out only once.
   In particular, with respect to our code base, you will rarely have
   to touch it, if at all.
 -}

-- Turn computations in "our" Sem monad into computations
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
    -- running the Sem computation, i.e. "scraping" off all effects
    -- by supplying appropriate interpreters/runners
    run request conn =
      request
-- the one piece that might change due to changes to the API:
-- E.g. if you'd use a handler that depends on an effect that
-- was not used before, you would have to extend the list of
-- interpreters accordingly
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
    -- translate the Sem computation into something Servant expects,
    -- that is a computation running in Servant's Handler monad.
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
