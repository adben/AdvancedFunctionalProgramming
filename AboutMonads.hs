{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}

module Assignment4 where


-- import Control.Applicative (Applicative(..))
-- import Control.Monad       (ap, liftM, replicateM)
-- import Data.Foldable       (Foldable(..))
-- import Data.Monoid         (Monoid(..), Sum(..), (<>))
-- import Data.Ratio          ((%))

import System.Random       (Random(random, randomR), getStdRandom)

-- https://uu-afp.github.io/as1.html
-- The Gambling Monad

data Coin = H | T
  deriving (Eq, Enum, Ord, Show)

data Dice = D1 | D2 | D3 | D4 | D5 | D6
  deriving (Eq, Enum, Ord, Show)


data Outcome = Win | Lose
  deriving (Eq, Ord, Show)

class Monad m => MonadGamble m where
  toss :: m Coin
  roll :: m Dice

{-|
Exercise 1: Write a function game ::MonadGamble m => m Outcome that implements the game above
-}
game :: MonadGamble m => m Outcome
game = undefined

-- Simulation
-- mhttps://uu-afp.github.io/as1.html#simulation
-- Exercise 2: Give Random instances for Coin and Dice.
-- Exercise 3: Give a MonadGamble instance for the IO monad.
-- Exercise 4: Write a function

instance Random Coin where
    randomR (l,h) g = undefined
    random = undefined

instance Random Dice where
    randomR (l, h) g = undefined
    random = undefined

instance MonadGamble IO where
  toss = undefined
  roll = undefined

