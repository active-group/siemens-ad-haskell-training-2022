module MikeLib where

x :: Integer
x = 10

y :: Integer
y = x + 10

-- double a number
f :: Integer -> Integer
-- >>> f 12
-- 24
f n = n * 2

-- design recipes

-- data definition

-- A pet is one of the following:
-- - cat - OR -
-- - dog - OR -
-- - spider

-- new data type
-- 3 cases
data Pet = Cat | Dog | Spider
  deriving Show -- indentation-sensitive!

-- intuition: capitalized words are for "constants"
-- lower case: variables

-- is a pet cute?
isCute :: Pet -> Bool
-- one equation per case
-- >>> isCute Cat
-- True
-- >>> isCute Spider
-- False

-- input has several cases =>
-- template has one equation per case
isCute Cat = True
isCute Dog = True
isCute Spider = False

-- Animals on the Texas highway

-- An animal is the one the following:
-- - armadillo - OR -
-- - parrot

-- Haskell: all cases have to be part of the same data definition

-- Armadillo has the following properties:
-- - alive - OR - dead? - AND -
-- - weight

data Liveness = Alive | Dead
  deriving Show

-- type synonym
type Weight = Integer

{-
-- record type
data Dillo = MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  deriving Show

-- each case always needs a constructor!

-- dilloLiveness: selector function

-- live armadillo, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- Dillo: state of the armadillo *at a certain time*

runOverDillo :: Dillo -> Dillo
-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- >>> runOverDillo dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}
{-
runOverDillo dillo =
    -- template for compound data as input
    -- dilloLiveness dillo ... dilloWeight dillo

    -- template for compound data as output
    -- MkDillo { dilloLiveness = ..., dilloWeight = ... }
    MkDillo {dilloLiveness = Dead, dilloWeight = dilloWeight dillo}
-}
-- other ways of writing same function:
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- pattern matching
-- runOverDillo (MkDillo { dilloLiveness = _liveness, dilloWeight = w }) =
--     MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo _liveness w) = MkDillo Dead w
-- functional update, make a copy with some attributes different
runOverDillo dillo = dillo { dilloLiveness = Dead }
-}

-- Wallclock time:
data Time = MkTime { timeHours :: Integer, timeMinutes :: Integer }
  deriving Show

time1 :: Time
-- 11:10AM
time1 = MkTime 11 10

-- How many minutes since midnight?
timeMinutesSinceMidnight :: Time -> Integer
-- >>> timeMinutesSinceMidnight time1
-- 670
timeMinutesSinceMidnight time =
-- template: derived from the type signature / datatype definitions
-- in this case: compound data as input
-- template: calls to the selectors
--    timeHours time      timeMinutes time
  (timeHours time) * 60 + (timeMinutes time)

-- make time object from minutes since midnight
msmToTime :: Integer -> Time
-- >>> msmToTime 670
-- MkTime {timeHours = 11, timeMinutes = 10}
msmToTime minutes =
    -- template: compound data / record as output
    -- call the constructor
--    MkTime { timeHours = undefined, timeMinutes = undefined }
    MkTime {timeHours = div minutes 60, timeMinutes = mod minutes 60 }

-- An animal is the one the following:
-- - armadillo - OR -
-- - parrot

-- A parrot has the following properties:
-- - sentence
-- - weight

-- algebraic data type
-- - multiple cases
-- - each case is compound data / has multiple attributes

-- 2 things called "Thickness":
data Thickness = Thickness Integer
--   ^^^^^^^^^ type / compile-time
--               ^^^^^^^^^ constructor / run-time
  deriving Show
data Length = Length Integer
  deriving Show

data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight}
  | MkParrot String Weight
  | MkSnake Thickness Length -- no Weight attribute!
  deriving Show


dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10}

dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "Hello!" 1

parrot2 :: Animal
parrot2 = MkParrot "Goodbye!" 2

-- Snake, 10cm thick, 200cm long
snake1 :: Animal
snake1 = MkSnake (Thickness 10) (Length 200)

-- run over a animal
runOverAnimal :: Animal -> Animal
-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1

-- >>> runOverAnimal snake1
-- MkSnake 0 200

-- "_": don't care
runOverAnimal (MkDillo _ weight) = MkDillo Dead weight
runOverAnimal (MkParrot _ weight) = MkParrot "" weight
runOverAnimal (MkSnake _thickness length) =
    MkSnake (Thickness 0) length

-- feed animal
-- >>> (feedAnimal dillo1) 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- >>> feedAnimal dillo2 5
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}

-- Haskell: function always has one input, one output
feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal (MkDillo liveness weight) amount =
    case liveness of
        Alive -> MkDillo liveness (weight + amount)
        Dead -> MkDillo liveness weight
feedAnimal (MkParrot sentence weight) amount =
    MkParrot sentence (weight + amount)
feedAnimal (MkSnake (Thickness t) length) amount =
    MkSnake (Thickness (t + div amount 2)) length


-- Exercise:
-- add another sort of animal: snakes, defined by thickness and length
-- also extend runOverAnimal, feedAnimal
-- examples for snakes and calling runOverAnimal, feedAnimal

feedAnimal' :: (Animal, Weight) -> Animal
-- >>> feedAnimal' (dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
feedAnimal' (MkDillo liveness weight, amount) =
    case liveness of
        Alive -> MkDillo liveness (weight+amount)
        Dead -> MkDillo liveness weight
feedAnimal' (MkParrot sentence weight, amount) =
    MkParrot sentence (weight+amount)
feedAnimal' (MkSnake (Thickness t) length, amount) =
    MkSnake (Thickness (t + div amount 2)) length

-- : 3-tuple
t1 :: (Bool, Pet, Animal)
t1 = (True, Spider, dillo1)

-- use it for functions that have multiple return values

-- A river is one of the following:
-- - a creek, originates at a spring at a certain place
-- - a confluence, flows together from two rivers at a certain place

-- A creek has the following properties:
-- - origin

-- A confluence has the following properties:
-- - place
-- - main stem <--- river
-- - tributary <--- river

type Place = String

data River =
    Creek Place
  | Confluence Place River River
  --                 ^^^^^ self-reference
  deriving Show

eschach :: River
eschach = Creek "Heimliswald"

prim :: River
prim = Creek "Dreifaltigkeitsberg"

neckar1 :: River
neckar1 = Confluence "Rottweil" eschach prim

schlichem :: River
schlichem = Creek "Tieringen"

neckar2 :: River
neckar2 = Confluence "Epfendorf" neckar1 schlichem

-- Does water flow from a place into a river?
flowsFrom :: Place -> River -> Bool
-- >>> flowsFrom "Tieringen" neckar2
-- True

-- >>> flowsFrom "Tieringen" prim
-- False

flowsFrom place (Creek origin) = place == origin
flowsFrom place (Confluence location mainStem tributary) =
     (place == location)
     -- template: for each self-reference put in a recursive call
     || (flowsFrom place mainStem)
     || (flowsFrom place tributary)

-- A shower product is one the following:
-- - soap (color)
-- - shampoo (hair type)
-- - 50/50 mixture of two shower products

-- - write a data type for shower products
-- - write a function that calculates the proportion of soap