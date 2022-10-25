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

-- Armadillo has the following properties:
-- - alive - OR - dead? - AND -
-- - weight

data Liveness = Alive | Dead
  deriving Show

-- type synonym
type Weight = Integer

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
