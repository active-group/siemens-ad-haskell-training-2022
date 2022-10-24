module Anomaly.Demo.Main (main) where

import Anomaly.Demo.Server (Config (..))
import qualified Anomaly.Demo.Server as Server
import qualified Anomaly.Demo.SqliteDB as Sqlite

-- | Bring up the database and start the server.
runDemo :: Config -> IO ()
runDemo config@(Config _ path) = do
  putStrLn $ "Setting up database " <> path
  Sqlite.setupDbTable path
  Server.runServer config

-- | Run the demo service.
main :: IO ()
main = runDemo Config{port = 8999, dbfile = "./demo-service.sqlite"}
