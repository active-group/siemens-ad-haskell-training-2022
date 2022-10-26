{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs, FlexibleContexts, PolyKinds, TypeOperators, DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module BiancaLib where

import Data.Function ((&))
import qualified Database.SQLite.Simple as Sqlite
import Polysemy (Embed, Member, Sem, embed)
import Polysemy.Internal (send)
import qualified Polysemy


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

data DBCommand a
  = Put Key Value (() -> DBCommand a)
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

{- yet another example: Teletype -}

data TTYCommand a
  = ReadTTY (String -> TTYCommand a)
  | WriteTTY String (() -> TTYCommand a)
  | ExitSuccess a

readTTY :: TTYCommand String
readTTY = ReadTTY (\ input -> ExitSuccess input)

writeTTY :: String -> TTYCommand ()
writeTTY output = WriteTTY output (\ () -> ExitSuccess ())

exitTTY :: a -> TTYCommand a
exitTTY result = ExitSuccess result

spliceTTY :: TTYCommand a -> (a -> TTYCommand b) -> TTYCommand b
spliceTTY (ReadTTY cb) next =
    ReadTTY (\x -> spliceTTY (cb x) next)
spliceTTY (WriteTTY output cb) next =
    WriteTTY output (\x -> spliceTTY (cb x) next)
spliceTTY (ExitSuccess result) next =
    next result

instance Functor TTYCommand where
instance Applicative TTYCommand where

instance Monad TTYCommand where
    (>>=) = spliceTTY
    return = ExitSuccess


{- Apparently, there are some similarities:
    + Both types have a "base case" that concludes a computation,
      i.e., Done and ExitSuccess, and, thus, does not take a callback.
    + All other constructors contain a callback that defines how the
      computation should continue -- consequently, it is also called
      a continuation.
      This callback takes the result of the previous computation
      and binds it to a local variable. Allowing subsequent computations
      to refer to it by name.
    + splice (aka bind) decends (recurses) into the callback until it reaches
      the respective base case where it applies its second argument, i.e.,
      the function that yields the subsequent computation(s).
      In fact, the callback is the only sensible place where recursion might
      occur since the callback is the only carrier of a self reference
      (at the type level).

    So, the mechanics used to "chain" multiple computation into a sequence
    is basically the same. This common behaviour, and base case in conjunction
    with a bind function that chains computation together, is what a monad
    defines in an abstract way.
-}

type MyMonad a = DBCommand (TTYCommand a)

-- f :: String -> MyMonad Int
-- f = do
--     writeTTY "hi there"
--     x <- get "Johannes"
--     writeTTY "got johannes"
--     return 5

class Monad m => MyDB m where
    get' :: String -> m Int
    put' :: String -> Int -> m ()

class Monad m => MyTeletype m where
    something :: m String


f'' :: (MyDB m, MyTeletype m) => m Int
f'' = do
    x <- get' "johannes"
    _ <- something
    return (x + 2)

{- organizing monadic code using Polysemy -}

data Store m a where
    InsertValue :: Key -> Value -> Store m ()
    LookupKey :: Key -> Store m (Maybe Value)

insertValue :: Member Store r => Key -> Value -> Sem r ()
insertValue key value = send (InsertValue key value)

lookupKey :: Member Store r => Key -> Sem r (Maybe Value)
lookupKey key = send (LookupKey key)

g :: Member Store r => Sem r (Maybe Value)
g = do
    x <- lookupKey "Johannes"
    return x

replaceValue :: Sqlite.Connection -> Key -> Value -> IO ()
replaceValue conn key value =
    Sqlite.execute conn "replace into test (key, value) values (?,?)" (key, value)

selectKey :: Sqlite.Connection -> Key -> IO (Maybe Value)
selectKey conn key = do
    res <- Sqlite.query conn "select value from test where key = ?" (Sqlite.Only key)
    case res of
      (Sqlite.Only result) : _ -> pure $ Just result
      [] -> pure Nothing

runStoreAsSqlite ::
  Member (Embed IO) r =>
  Sqlite.Connection ->
  Sem (Store ': r) a -> Sem r a
runStoreAsSqlite conn =
    Polysemy.interpret (\ program ->
        case program of
            InsertValue key value -> embed (replaceValue conn key value)
            LookupKey key -> embed (selectKey conn key)
    )


createDbTable :: Sqlite.Connection -> IO ()
createDbTable conn =
  let statement = "create table if not exists test (key text primary key not null, value integer)"
   in Sqlite.execute_ conn statement


program :: String -> IO ()
program dbfile = do
    conn <- Sqlite.open dbfile
    createDbTable conn
    result <- p
        & runStoreAsSqlite conn
        & Polysemy.runM
    Sqlite.close conn
    print result

  where
    p :: Member Store r => Sem r (Maybe Integer)
    p = do
        insertValue "Bianca" 42
        x <- lookupKey "Bianca"
        return (x + 1)
      
