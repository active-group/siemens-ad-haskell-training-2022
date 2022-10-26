{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module BiancaLib where

{-
program:
    put "Bianca" 42
    put "Johannes" 16
    x = get "Johannes"
    y = get "Bianca"
    return (x + y)
-}

-- data DBCommand a =
--       Put String Integer
--     | Get String
--     | Done a

-- type DBProgram a = [DBCommand a]

-- p0 = [
--     Put "Bianca" 42
--     , Put "Johannes" 16
--     , Get "Johannes"
--     , Get "Bianca"
--     , Done ??? -- we can't name results
-- ]

type Key = String
type Value = Integer

data DBCommand a =
      Put Key Value (() -> DBCommand a)
    | Get Key (Value -> DBCommand a)
    | Done a

put :: Key -> Value -> DBCommand ()
put key value = Put key value (\ () -> Done ())

get :: Key -> DBCommand Value
get key = Get key (\ value -> Done value)

p1 =
    Put "Bianca" 42 (\ () ->
        Put "Johannes" 16 ( \ () ->
            Get "Johannes" (\ x ->
                Get "Bianca" (\ y ->
                    Done (x + y)
                    )
                )
            )
        )

splice :: DBCommand a -> (a -> DBCommand b) -> DBCommand b
splice (Put key value callback) f =
    Put key value ( \ () -> splice (callback ()) f)
splice (Get key callback) f =
    Get key (\ value -> splice (callback value) f)
splice (Done result) f = f result

p2 =
    put "Bianca" 42 `splice` (\ _ ->
    put "Johannes" 16 `splice` (\ _ ->
    get "Johannes" `splice` (\ x ->
    get "Bianca" `splice` (\ y ->
    Done (x + y)))))

-- >>> :i Monad
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   return :: a -> m a

-- instance Monad (DBCommand a) where

-- >>> :t Done
-- Done :: a -> DBCommand a
-- >>> :t splice
-- splice :: DBCommand a -> (a -> DBCommand b) -> DBCommand b

instance Functor DBCommand where
instance Applicative DBCommand where

instance Monad DBCommand where
    (>>=) = splice
    return = Done

p3 =
    put "Bianca" 42 >>= (\ _ ->
    put "Johannes" 16 >>= (\ _ ->
    get "Johannes" >>= (\ x ->
    get "Bianca" >>= (\ y ->
    return (x + y)))))

-- do notation
p3' = do
    put "Bianca" 42
    put "Johannes" 16
    x <- get "Johannes"
    y <- get "Bianca"
    return (x + y)

--

