{-# LANGUAGE DerivingStrategies #-}

module Anomaly.Demo.Domain (
  LookupError (..),
  UpdateResponse (..),
  AspectName (..),
  ColorRibbonId (..),
) where

import Data.Text (Text)

newtype LookupError = NotFound AspectName
  deriving stock (Eq, Show)

data UpdateResponse = ACRLinkUpdated | ACRLinkCreated
  deriving stock (Eq, Show)

-- | The name of an aspect.
-- An aspect is uniquely identified by its name.
newtype AspectName = AspectName {aspectName :: Text}
  deriving stock (Eq, Show, Ord)

-- | The id of a color ribbon.
-- Since we don't model color ribbons in the backend (for now) this is really all there is
-- to color ribbons, as far as the backend is concerned.
-- The format (numeric/alpha-numeric/whatever) is determined solely by the client, which,
-- in turn, knows the actual content of a particular ribbon in terms of associated colors.
newtype ColorRibbonId = ColorRibbonId {colorRibbonId :: Text}
  deriving stock (Eq, Show)
