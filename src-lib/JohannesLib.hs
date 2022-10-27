{-# LANGUAGE InstanceSigs, OverloadedStrings, DerivingStrategies, DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
module JohannesLib where

import Data.Aeson
import GHC.Generics (Generic)
-- import Control.Lens

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


------------------------ Day 3 --------------------

-- optics, specifically lenses

data Person = Person
    { name :: String
    , address :: Address
    , age :: Integer
    }
    deriving (Show, Generic) -- Generic added later

data Address = Address
    { street :: Street
    , country :: String
    -- , name :: String -- clashes -> DuplicateRecordFields
    }
    deriving (Show, Generic) -- Generic added later

-- convention: use unNewtypeName often
newtype Street = Street {unStreet :: String}
    deriving (Show, Generic) -- Generic added later

johannes :: Person
johannes =
    Person "Johannes" (Address (Street "My Street") "DE") 36

-- >>> johannes{ name = "Jo" }
-- Person {name = "Jo", address = Address {street = Street {unStreet = "My Street"}, country = "DE"}, age = 36}

addStreetNumber :: Person -> Integer -> Person
addStreetNumber person n =
    person{ address =
        let oldAddr = address person  -- let creates local variables: syntax: let foo = "three" in foo ++ "hi"
            Street streetName = street oldAddr -- pattern matching on the LHS
        in  oldAddr { street = Street $ streetName ++ " " ++ show n}}

-- imperative: person.address.street.name = person.address.street + " " + n

data Lens a s = Lens -- s is structure, a is the type of value
    { get :: s -> a
    , set :: s -> a -> s
    }

streetNameL :: Lens String Person
streetNameL = Lens
    (unStreet . street . address)
    (\ p val -> p{ address = let addr = address p in addr{street = Street val}})

-- >>> get streetNameL johannes
-- "My Street"

-- >>> set streetNameL johannes "foo"
-- Person {name = "Johannes", address = Address {street = Street {unStreet = "foo"}, country = "DE"}, age = 36}

-- not all the way there to be able to add street numbers

over :: Lens a s -> (a -> a) -> s -> s -- think: map
over lens f s =
    let oldA = get lens s
    in set lens s (f oldA)

-- >>> over streetNameL (\ s -> s ++ " 17") johannes
-- Person {name = "Johannes", address = Address {street = Street {unStreet = "My Street 17"}, country = "DE"}, age = 36}

composeL :: Lens inner outer -> Lens b inner -> Lens b outer
composeL lensOuter lensInner = Lens
    -- (\ outer -> get lensInner (get lensOuter outer))
    (get lensInner . get lensOuter)
    (\ outer val -> 
        let newInner = set lensInner (get lensOuter outer) val 
        in set lensOuter outer newInner)

-- more optics:
-- prisms (failure to look at certain parts)
-- traversals (look at multiple things at once)

-- we use the `lens` library
-- generic-lens: creates lenses for us by deriving Generic

-- lenses:
--   get => view
--   set => set

-- >>> view #name johannes
-- Variable not in scope: view :: Person -> t0 -> t

-- rule: take the selector name, prepend a '#' -> this is the lens
-- prisms: data Foo = Foo String | Bar Int deriving Generic
--                   #_Foo         #_Bar
--
-- whenever you see # in our codebase:
-- generic-lens creating lenses/prisms

-- in the actual code:
-- view #name johannes
-- view (#address . #street . #unStreet) johannes
-- so:   use . to compose lenses on the spot
-- set (#address . #street . #unStreet) johannes "hi"

-- alternative syntax:

-- johannes ^. #name     <- ^. is infix view

-- johannes & #name .~ "my new name"
--          & #address . #street . #unStreet .~ "my new street"
--          & #age %~ (\ n -> n + 2)

-- most common ones:
-- ^.    view
-- .~    set
-- %~    over
-- ^?    preview

-- (   (&) is reverse function application)