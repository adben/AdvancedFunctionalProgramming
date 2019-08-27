{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}

module AboutMonads where


-- import Control.Applicative (Applicative(..))
import Control.Monad       (ap, liftM, replicateM)
-- import Data.Foldable       (Foldable(..))
-- import Data.Monoid         (Monoid(..), Sum(..), (<>))
-- import Data.Ratio          ((%))

import System.Random       (Random(random, randomR), getStdRandom)

-- https://uu-afp.github.io/as1.html
-- The Gambling Monad

data Coin = H | T
  deriving (Bounded, Eq, Enum, Ord, Show)

data Dice = D1 | D2 | D3 | D4 | D5 | D6
  deriving (Bounded, Eq, Enum, Ord, Show)

data Outcome = Win | Lose
  deriving (Eq, Ord, Show)

class Monad m => MonadGamble m where
  toss :: m Coin
  roll :: Int -> m Int

{-|
Exercise 1: Write a function game ::MonadGamble m => m Outcome that implements the game above
-}
game :: MonadGamble m => m Outcome
game = do
  x <- replicateM 6 toss -- TODO what type?
  let i = length (filter (== H) x)
  y <- roll 6
  if y > i
    then return Win
    else return Lose

--game = replicateM 10 toss -- TODO 

-- Simulation
-- mhttps://uu-afp.github.io/as1.html#simulation
-- Exercise 2: Give Random instances for Coin and Dice.
-- Exercise 3: Give a MonadGamble instance for the IO monad.
-- Exercise 4: Write a function that runs a game of chance (given as the first parameter, not necessarily the game implemented in Exercise 1) $n$ times($n > 0$, the second parameter) and returns the fraction of games won. You can now approximate to probability of winning using simulate game 10000

instance Random Coin where
    randomR (l,h) g = undefined
    random = undefined

instance Random Dice where
    randomR (l, h) g = undefined
    random = undefined

instance MonadGamble IO where
  toss = undefined
  roll = undefined

simulate :: IO Outcome -> Integer -> IO Rational
simulate = undefined

