{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Util
    ( Day
    , runDay'
    , Task
    , runTask'
    , SomeDay(..)
    , runSomeDay
    , runDay
    , runTask
    , splitOnDoubleNewline
    , trim
    , toSnd
    , split
    , uncurry3
    , uncurry4
    , chainl1
    ) where

import Control.Applicative ((<**>), Alternative(..))
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Proxy (Proxy(..))
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.TypeLits (KnownNat, Nat, natVal)


---------------------------------------------
-- * Nifty monads to automate console logging
---------------------------------------------

-- | Exsistential type to hold each day, since their day numbers are a part of
-- their type.
data SomeDay = forall n . KnownNat n => SomeDay (Day n ())

runSomeDay :: SomeDay -> IO ()
runSomeDay (SomeDay day) = runDay day

-- | A given day with its day number.
newtype Day (n :: Nat) a = Day { runDay' :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO) via IO

-- | Run a day, logging the day number.
runDay :: forall n a . KnownNat n => Day n a -> IO a
runDay m = do
    putStrLn $ "Day " <> show (natVal (Proxy @n))
    runDay' m

-- | A day's task with the task number. Usually 1 or 2, since Advent of Code
-- only has 2 tasks per day.
newtype Task (n :: Nat) a = Task { runTask' :: IO a }
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO) via IO

-- | Run a task and time its execution, rather roughly, logging the execution
-- time.
runTask :: forall n m a
         . (KnownNat n, Show a, NFData a)
        => Task n a
        -> Day m ()
runTask task = do
    startTime <- liftIO getPOSIXTime
    !res      <- liftIO $ evaluate . force =<< runTask' task
    endTime   <- liftIO getPOSIXTime
    liftIO
        $  putStrLn
        $  "    Task "
        <> taskNum
        <> ": "
        <> show res
        <> "\n    Task "
        <> taskNum
        <> ": Took "
        <> show (endTime - startTime)
    where taskNum = show (natVal (Proxy @n))


-----------------
-- * Actual utils
-----------------

splitOnDoubleNewline :: String -> [String]
splitOnDoubleNewline = go []
  where
    go acc []                   = [reverse acc]
    go acc ('\n' : '\n' : rest) = reverse acc : go [] rest
    go acc (x           : rest) = go (x : acc) rest

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
{-# INLINE trim #-}

split :: Char -> String -> [String]
split _ "" = []
split c s  = cons $ case break (== c) s of
    (l, s') ->
        ( l
        , case s' of
            []      -> []
            _ : s'' -> split c s''
        )
    where cons ~(h, t) = h : t

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)
{-# INLINE toSnd #-}

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
{-# INLINE uncurry3 #-}

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d
{-# INLINE uncurry4 #-}

chainl1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainl1 p op = p <**> rst
    where rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id
{-# INLINE chainl1 #-}
