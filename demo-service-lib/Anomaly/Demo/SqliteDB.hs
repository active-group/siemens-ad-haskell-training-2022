{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Anomaly.Demo.SqliteDB (
  selectRibbon,
  insertOrUpdateRibbon,
  deleteRibbon,
  createDbTable,
  setupDbTable,
  tableName,
  DB (..),
) where

import Anomaly.Demo.Domain
import Data.String (IsString)
import Data.Text (Text)
import qualified Database.SQLite.Simple as Sqlite
import qualified Database.SQLite.Simple.FromField as Sqlite
import qualified Database.SQLite.Simple.ToField as Sqlite

-- | The name of the table containing the aspect to color ribbon mappings.
tableName :: (IsString s, Semigroup s) => s
tableName = "acr_map"

-- | Tag around domain types used to attach unique 'Postgres.FromField'
-- and 'Postgres.ToField' instances.
newtype DB a = DB {unDB :: a}
  deriving stock (Eq, Show)

deriving via Text instance Sqlite.FromField (DB AspectName)

deriving via Text instance Sqlite.ToField (DB AspectName)

deriving via Text instance Sqlite.FromField (DB ColorRibbonId)

deriving via Text instance Sqlite.ToField (DB ColorRibbonId)

-- | Insert or update the 'ColorRibbonId' associated with a given 'AspectName'.
insertOrUpdateRibbon :: Sqlite.Connection -> AspectName -> ColorRibbonId -> IO UpdateResponse
insertOrUpdateRibbon conn aspect ribbon =
  Sqlite.withImmediateTransaction conn upsert
  where
    sqlstmt =
      "insert into " <> tableName
        <> " (aspect, ribbon) \
           \ values (?,?) on conflict(aspect) \
           \ do update set ribbon = excluded.ribbon"
    upsert = do
      prevIns <- Sqlite.lastInsertRowId conn
      Sqlite.execute conn sqlstmt (DB aspect, DB ribbon)
      lastIns <- Sqlite.lastInsertRowId conn
      if prevIns /= lastIns
        then return ACRLinkCreated
        else return ACRLinkUpdated

-- | Create table for aspect-to-color-ribbon map if it doesn't exist yet.
createDbTable :: Sqlite.Connection -> IO ()
createDbTable conn =
  let sqlstmt =
        "create table if not exists " <> tableName
          <> " ( aspect text primary key not null\
             \ , ribbon text not null\
             \ )"
   in Sqlite.execute_ conn sqlstmt

setupDbTable :: FilePath -> IO ()
setupDbTable dbfile = Sqlite.withConnection dbfile createDbTable

-- | Select the 'ColorRibbonId' linked to a particular 'AspectName', if defined.
selectRibbon :: Sqlite.Connection -> AspectName -> IO (Maybe ColorRibbonId)
selectRibbon conn aspect =
  let sqlstmt = "select ribbon from " <> tableName <> " where aspect = ?"
   in do
        r <- Sqlite.query conn sqlstmt (Sqlite.Only (DB aspect))
        case r of
          [] -> return Nothing
          (Sqlite.Only (DB ribbon)) : _ -> return (Just ribbon)

-- | Delete a 'ColorRibbonId' link.
deleteRibbon :: Sqlite.Connection -> AspectName -> IO ()
deleteRibbon conn aspect =
  let sqlstmt = "delete from " <> tableName <> " where aspect = ?"
   in Sqlite.execute conn sqlstmt (Sqlite.Only (DB aspect))
