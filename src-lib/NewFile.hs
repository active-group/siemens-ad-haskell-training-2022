module NewFile where

module Lenses where

data Person = Person
  { name :: String
  , address :: Address
  , age :: Integer
  }
  deriving Show

data Address = Address
  { street :: Street
  , country :: String
  }
  deriving Show

newtype Street = Street {unStreet :: String}
  deriving Show

johannes :: Person
johannes = Person "Johannes" (Address (Street "Waldhausen") "DE") 36

addStreetNumber :: Person -> Integer -> Person
addStreetNumber person n = 
    person { address = 
        let addr = address person in addr { street =
            let (Street streetName) = street addr in
                Street (streetName <> " " <> show n) } }

-- imperative code: person.address.street.name = person.address.street + " " + number
-- geht leider nicht ganz... f.bar.blub gibt's nicht -> können wir abstrahieren?

data Lens a b = Lens
  { get :: b -> a
  , set :: b -> a -> b
  }

streetNameL :: Lens String Person
streetNameL = Lens 
    (\ p -> unStreet (street (address p)))
    (\ p val -> p{ address = let addr = address p in addr { street = Street val } })

-- >>> get streetNameL johannes
-- "Waldhausen"

-- >>> set streetNameL johannes "street"
-- Person {name = "Johannes", address = Address {street = Street {unStreet = "street"}, country = "DE"}, age = 36}

-- still unable to add street number!

-- using get and set, can we do this? EXERCISE?

-- think: map "over" a structure at a certain point
over :: Lens a b -> (a -> a) -> b -> b
over lens f b =
    let oldA = get lens b
    in set lens b (f oldA)

-- >>> over streetNameL (\ s -> s ++ " 17") johannes
-- Person {name = "Johannes", address = Address {street = Street {unStreet = "Waldhausen 17"}, country = "DE"}, age = 36}



-- Prisms?

data Prism a b = Prism
    { preview :: }