{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}

module Util
    ( Day
    , Task
    , SomeDay(..)
    , runSomeDay
    , runDay
    , runTask
    , splitOnDoubleNewline
    , trim
    , split
    ) where

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

data SomeDay = forall n . KnownNat n => SomeDay (Day n ())

runSomeDay :: SomeDay -> IO ()
runSomeDay (SomeDay day) = runDay day

newtype Day (n :: Nat) a = Day { runDay' :: IO a }
    deriving (Functor, Applicative, Monad) via IO

instance MonadIO (Day n) where
    liftIO = Day

runDay :: forall n a . KnownNat n => Day n a -> IO a
runDay m = do
    putStrLn $ "Day " <> show (natVal (Proxy @n))
    runDay' m

newtype Task (n :: Nat) a = Task { runTask' :: IO a }
    deriving (Functor, Applicative, Monad, MonadFail) via IO

instance MonadIO (Task n) where
    liftIO = Task

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
