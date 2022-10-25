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
data Pet = Cat | Dog | Spider
  deriving Show -- indentation-sensitive!


