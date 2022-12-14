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
  deriving (Show) -- indentation-sensitive!

instance Eq Pet where
    (==) = eqPet

eqPet :: Pet -> Pet -> Bool
eqPet Cat Cat = True
eqPet Dog Dog = True
eqPet Spider Spider = True
eqPet _ _ = False


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
  deriving (Show, Eq)

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
  deriving (Show, Eq)

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
  deriving (Show, Eq)
--   ^^^^^^^^^ type / compile-time
--               ^^^^^^^^^ constructor / run-time
data Length = Length Integer
  deriving (Show, Eq)

data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight}
  | MkParrot String Weight
  | MkSnake Thickness Length -- no Weight attribute!
  deriving (Show, Eq)


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

-- >>> feedAnimal parrot1 1
-- MkParrot "Hello!" 2

-- >>> feedAnimal parrot2 5
-- MkParrot "Goodbye!" 2

-- Haskell: function always has one input, one output
feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal (MkDillo liveness weight) amount =
    case liveness of
        Alive -> MkDillo liveness (weight + amount)
        Dead -> MkDillo liveness weight
feedAnimal (MkParrot sentence weight) amount =
-- parrots can not be >2kg
    if weight + amount > 2
    then MkParrot sentence weight
    else MkParrot sentence (weight + amount)
feedAnimal (MkSnake (Thickness t) length) amount =
    MkSnake (Thickness (t + div amount 2)) length

-- feedAnimal would be more convenient if the parameters were the other way around.

swap :: (a -> b -> c) -> (b -> (a -> c))
-- f :: (a -> b -> c)
-- have to return (b -> a -> c)
swap f =
  \ b -> \ a -> f a b

-- built-in as uncurry
tuplify :: (a -> b -> c) -> ((a, b) -> c)
tuplify f =
    \ (a, b) -> f a b

-- built-in curry after Haskell Curry
untuplify :: ((a, b) -> c) -> (a -> b -> c)
untuplify f =
    \ a -> \ b -> f (a, b)

-- built-in under flip

feedAnimal2 :: Weight -> Animal -> Animal
feedAnimal2 = swap feedAnimal

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
     ||??(flowsFrom place mainStem)
     || (flowsFrom place tributary)

-- A shower product is one the following:
-- - soap (color) - OR -
-- - shampoo (hair type) - OR -
-- - 50/50 mixture of two shower products
--                        ^^^^^^^^^^^^^^ 2 self references

-- - write a data type for shower products
-- - write a function that calculates the proportion of soap

data Color = Blue | Blonde | Red
  deriving Show

data Hairtype = Normal | Dandruff | Fine | Oily
  deriving Show

data Product =
    Soap Color
  | Shampoo Hairtype
  | Mixture Product Product
  deriving Show

-- propertion of soap in shower product (100% = 1)
soapProportion :: Product -> Double
soapProportion (Soap _color) = undefined
soapProportion (Shampoo _hairType) = undefined
soapProportion (Mixture product1 product2) =
    (soapProportion product1) * 0.5
    + (soapProportion product2) * 0.5

-- A list is one the following:
-- - the empty list
-- - a cons list, consisting of the first element and the rest list
--                                                             ^^^^

{-
data ListOfIntegers =
    Empty
  | Cons Integer ListOfIntegers
  deriving Show
-}

{-
-- abstract:
data ListOf element -- ListOf is a type *constructor*
  = Empty
  | Cons element (ListOf element)
  deriving (Show)


-- 1-element list: 5
list1 :: ListOf Integer
list1 = Cons 5 Empty
-- 2-element list: 2 5
list2 :: ListOf Integer
list2 = Cons 2 (Cons 5 Empty)
-- 3-element list: 4 2 5
list3 :: ListOf Integer
list3 = Cons 4 (Cons 2 (Cons 5 Empty))
-- 4-element list: 7 4 2 5
list4 :: ListOf Integer
list4 = Cons 7 list3

-- add the elements of a list
listSum :: ListOf Integer -> Integer
-- >>> listSum list4
-- 18
listSum Empty = 0
listSum (Cons first rest) =
    first + listSum rest
-}

-- Built-in lists:
-- - empty list: []
-- - cons: : (infix)

list1 :: [Integer]
list1 = 5 : []
list2 :: [Integer]
list2 = 2 : (5 : [])
-- abbreviation:
list2' :: [Integer]
list2' = [2,5]
list3 :: [Integer]
list3 = [4, 2, 5]
list4 :: [Integer]
list4 = 7 : list3

listSum :: [Integer] -> Integer
-- >>> listSum list4
-- 18
listSum [] = 0
-- in Haskell: head / tail
listSum (first:rest) =
    first + listSum rest

listProduct :: [Integer] -> Integer
listProduct [] = 1
listProduct (first:rest) =
    first * listProduct rest

-- "right fold": right-to-left
-- "universal list function"
listFold :: b -> (a -> b -> b) -> [a] -> b
-- >>> listFold 0 (+) [1,2,3,4,5,6]
-- 21
-- >>> listFold 1 (*) [1,2,3,4,5,6]
-- 720
-- >>> listFold [] (:) [1,2,3,4,5,6]
-- [1,2,3,4,5,6]
listFold e _op [] = e
listFold e op (first:rest) =
    op first (listFold e op rest)

highway :: [Animal]
highway = [dillo1, dillo2, parrot1, parrot2, snake1]

-- run over all animals in a list
runOverAnimals :: [Animal] -> [Animal]
runOverAnimals [] = []
runOverAnimals (first:rest) =
    (runOverAnimal first) : (runOverAnimals rest)

-- increment all numbers in a list
incList :: [Integer] -> [Integer]
-- >>> incList [1,2,3,4,5]
-- [2,3,4,5,6]
incList [] = []
incList (first:rest) =
    (inc first) : (incList rest)

inc :: Integer -> Integer
-- inc n = n + 1
-- is an abbreviation for:
inc = (\n -> n + 1)

listMap :: (a -> b) -> [a] -> [b]
-- >>> listMap inc [1,2,3,4,5]
-- [2,3,4,5,6]

-- (\ n -> n + 1) -- lambda expression -> function

-- >>> listMap (\ n -> n + 1) [1,2,3,4,5]
-- [2,3,4,5,6]
listMap _f [] = []
listMap f (first : rest) =
  (f first) : (listMap f rest)

-- built-in map

-- is a number even?
isEven :: Integer -> Bool
-- >>> isEven 4
-- True
-- >>> isEven 5
-- False
isEven n = mod n 2 == 0

isOdd :: Integer -> Bool
isOdd n = mod n 2 == 1

-- Exercise: write a function that extracts all even elements
-- from a list of integers
extractOdds :: [Integer] -> [Integer]
-- >>> extractOdds [1,2,3,4,5,6,7]
-- [1,3,5,7]
extractOdds [] = []
extractOdds (first:rest) =
    if isOdd first
    then first : (extractOdds rest)
    else extractOdds rest

-- abstract
-- 1. copy + rename (remember recursive calls)
-- 2. replace differences by abstract names
-- 3. add abstract name to the parameters + recursive calls

-- a: type variable (lower case)
-- each call can use a different type for a
-- higher-order function: accepts function as an argument
extract :: (a -> Bool) -> [a] -> [a]
-- >>> extract isEven [1,2,3,4,5,6,7]
-- [2,4,6]
-- >>> extract isOdd [1,2,3,4,5,6,7]
-- [1,3,5,7]
-- >>> extract isCute [Cat, Cat, Dog, Spider, Cat]
-- [Cat,Cat,Dog,Cat]
extract p [] = []
extract p (first : rest) =
  if p first
    then first : (extract p rest)
    else extract p rest

-- usually provided as filter

-- listIndex can produce a result - or not!
data Optional a =
    Null
  | Result a
  deriving Show -- <-- generates an instance of Show (Optional a) automatically

-- data Maybe a = Nothing |??Just a

-- Eq a: constraint on a, before =>
-- not any a, but one that supports comparison
{-
class Eq a where -- "interface"
  -- method of Eq
  (==) :: a -> a -> Bool

instance Eq Integer where
    (==) = ...
-}

-- implementation of a type class: instance

-- Eq specifically "type class", think "interface"

-- find the index of an element in a list
listIndex :: Eq a => a -> [a] -> Optional Integer
-- >>> listIndex 8 [5, 7, 8, 2, 9]
-- Result 2
-- >>> listIndex Cat [Dog, Cat, Spider]
-- Result 1
listIndex _x [] = Null
listIndex x (first:rest) =
    if first == x 
    then Result 0
    else fmap inc (listIndex x rest)
{-
       case listIndex x rest of
        Null -> Null
        Result index -> Result (index+1)
-}

{-
class Functor f where
  fmap ::      (a -> b) -> f      a -> f          b
-}

-- listMap ::  (a -> b) -> ListOf a   -> ListOf   b
--             (a -> b) -> f        a -> f        b
optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f Null = Null
optionalMap f (Result a) = Result (f a)

instance Functor Optional where
    fmap = optionalMap

-- built-in type classes:
-- Show, Eq, Ord (ordered), Num


f' :: Num a => a -> a
-- >>> f' 5.0
-- 6.0
-- >>> f' 6
-- 6

f' n = n + 1

natsFrom :: Integer -> [Integer]
natsFrom n = n : natsFrom (n + 1)
 -- n : n+1 : n+2 : ...

strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n list = filter (\ m -> mod m n /= 0) list

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (first:rest) = first : sieve (strikeMultiples first rest)

-- data Foo = Foo { bar :: !String } -- String = [Char]

{-
algebra:
- type T
- operations (with type signatures)
- laws
-}

-- a type with just an operation
class Semigroup' a where
  op :: a -> a -> a

-- String: instance Semigroup String where
--            op s1 s2 = s1 ++ s2

-- a type with an operation op, and a "neutral element", such that:
-- 
class Semigroup' a => Monoid' a where
  -- op :: a -> a -> a
  neutral :: a
  -- op neutral x == x
  -- op x neutral == x

-- "" ++ "abc" == "abc"
-- "abc" ++ "" == "abc"
-- so: instance Monoid String where
--       neutral = ""

instance Semigroup' a => Semigroup' (Optional a) where
  op Null Null = Null
  op (Result a) Null = Result a
  op Null (Result a) = Result a
  op (Result a) (Result b) = Result (op a b)

instance Semigroup' a => Monoid' (Optional a) where
  neutral = Null

-- op a (op b (op c (op d e)))

-- associativity:
-- op a (op b c) == op (op a b) c
-- a `op` (b `op` c) == (a `op` b) `op` c

-- a `op` b `op` c `op` d `op` e `op` f

-- intuition: Optional makes semigroups into monoids