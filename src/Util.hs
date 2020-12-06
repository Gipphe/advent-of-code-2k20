{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
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
    , safeIndex
    , sumTrue
    , every
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, Nat, natVal)

data SomeDay = forall n. KnownNat n => SomeDay (Day n ())

runSomeDay :: SomeDay -> IO ()
runSomeDay (SomeDay day) = runDay day

newtype Day (n :: Nat) a = Day (IO a)
    deriving (Functor, Applicative, Monad) via IO

instance MonadIO (Day n) where
    liftIO = Day

runDay :: forall n a . KnownNat n => Day n a -> IO a
runDay (Day act) = do
    putStrLn $ "Day " <> show (natVal (Proxy @n))
    act

newtype Task (n :: Nat) a = Task (IO a)
    deriving (Functor, Applicative, Monad) via IO

instance MonadIO (Task n) where
    liftIO = Task

runTask :: forall n m a . (KnownNat n, Show a) => Task n a -> Day m ()
runTask (Task act) = do
    res <- liftIO act
    liftIO
        $  putStrLn
        $  "    Task "
        <> show (natVal (Proxy @n))
        <> ": "
        <> show res

safeIndex :: Int -> [a] -> Maybe a
safeIndex 0 (x : _) = Just x
safeIndex _ []      = Nothing
safeIndex i (_ : xs)
    | i > 0     = safeIndex (i - 1) xs
    | otherwise = Nothing

sumTrue :: [Bool] -> Int
sumTrue = foldr (\x -> if x then (+ 1) else id) 0

every :: Int -> [a] -> [a]
every i' = every' i' i'
  where
    every' _ _ []       = []
    every' i 0 (x : xs) = x : every' i i xs
    every' i n (_ : xs) = every' i (n - 1) xs
