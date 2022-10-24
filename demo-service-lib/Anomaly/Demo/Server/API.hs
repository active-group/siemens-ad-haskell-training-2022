{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Anomaly.Demo.Server.API (
  Routes,
  API (..),
) where

import Anomaly.Demo.Domain
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.String (IsString)
import Data.Text (Text, unpack)
import Servant.API

-- | Tag around domain types used to attach unique 'Aeson.FromJSON'
-- and 'Aeson.ToJSON' instances.
newtype API a = API {unAPI :: a}
  deriving stock (Eq, Show)

type Routes =
  Get '[JSON] [API AspectName]
    :<|> Capture "name" (API AspectName)
      :> "color-ribbon"
      :> ( Get '[JSON] (API ColorRibbonId)
            :<|> DeleteNoContent
            :<|> ReqBody '[JSON] (API ColorRibbonId)
              :> UVerb 'PUT '[JSON] '[WithStatus 201 NoContent, WithStatus 204 NoContent]
         )

fromText :: IsString a => Semigroup a => a -> (Text -> b) -> Text -> Either a b
fromText name constr input
  | isEmpty = Left $ "Empty is not a valid " <> name
  | containsNonPrintable = Left $ "Invalid " <> name <> ": Input contains non-printable characters"
  | otherwise = Right (constr input)
  where
    isEmpty = input == ""
    containsNonPrintable = not . all Char.isPrint $ unpack input

fromTextOrFail :: MonadFail a => String -> (Text -> b) -> Text -> a b
fromTextOrFail name constr input =
  let eitherErrOrVal = fromText name constr input
   in case eitherErrOrVal of
        Left msg -> fail msg
        Right val -> pure val

-- | Any non-empty JSON string is considered a valid 'AspectName'.
-- Consequently, parsing an empty string will fail.
instance Aeson.FromJSON (API AspectName) where
  parseJSON =
    Aeson.withText "API AspectName" $
      fromTextOrFail "AspectName" (API . AspectName)

-- | Any non-empty URI segment is considered a valid 'AspectName'.
instance Servant.API.FromHttpApiData (API AspectName) where
  parseUrlPiece = fromText "AspectName" (API . AspectName)

-- | Any non-empty JSON string is considered a valid
-- 'ColorRibbonId'.
instance Aeson.FromJSON (API ColorRibbonId) where
  parseJSON =
    Aeson.withText "API ColorRibbonId" $
      fromTextOrFail "ColorRibbonId" (API . ColorRibbonId)

-- | Any non-empty URI segment is considered a valid 'ColorRibbonId'.
instance Servant.API.FromHttpApiData (API ColorRibbonId) where
  parseUrlPiece = fromText "ColorRibbonId" (API . ColorRibbonId)

deriving via Text instance Aeson.ToJSON (API AspectName)

deriving via Text instance Aeson.ToJSON (API ColorRibbonId)
