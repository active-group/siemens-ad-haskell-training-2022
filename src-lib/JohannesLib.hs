{-# LANGUAGE InstanceSigs, OverloadedStrings, DerivingStrategies, DerivingVia #-}
module JohannesLib where

import Data.Aeson

-- type Thickness = Int
-- type Length = Int

-- newtype: like data, but "no" runtime costs
newtype Thickness = MkThickness Int
    deriving Show
    deriving (FromJSON, ToJSON) via Int

newtype Length = MkLength Int
    deriving Show
    deriving (FromJSON, ToJSON) via Int

data Snake = Snake
  { snakeThickness :: Thickness
  , snakeLength :: Length
  }
  deriving Show

{-
pseudo C code:

typedef struct Thickness {
    thickness :: int
};
-}

feedSnake :: Snake -> Int -> Snake
feedSnake (Snake (MkThickness t) (MkLength l)) amount = 
    Snake 
      (MkThickness (t + amount))
      (MkLength (l + amount))


-- newtypes to attach type class instances

class Semigroup' a where
    op :: a -> a -> a

instance Semigroup' Int where
    op = (+)

newtype MyInt = MyInt Int

instance Semigroup' MyInt where
    op (MyInt a) (MyInt b) = MyInt $ a + 2 * b

-- >>> :type ($)
-- ($) :: (a -> b) -> a -> b
-- ($) f a = f a
-- f $ a = f a


-- serialize Snake to JSON
snakey :: Snake
snakey = Snake (MkThickness 2) (MkLength 3)

instance ToJSON Snake where
    toJSON :: Snake -> Value
    -- toJSON (Snake (MkThickness t) (MkLength l)) =
    toJSON (Snake t l) =
        -- typed hole: _
        object 
          [ --("thickness", t) -- String = [Char]
            "thickness" .= t
          , "length" .= l
          ]

-- found encode by crawling the documentation:
-- >>> encode snakey
-- "{\"length\":3,\"thickness\":2}"

instance FromJSON Snake where
    parseJSON = -- Parser a    -----> functional parsing or monadic parsing
      withObject "Snake" $ \ obj ->
        do
            t <- obj .: "thickness"
            l <- obj .: "length"
            return (Snake t l)