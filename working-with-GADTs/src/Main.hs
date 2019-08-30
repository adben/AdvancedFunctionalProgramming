{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandalineDeriving #-}

module Main where

import Control.Monad
import Data.Char
import Control.Applicative

data Expr where
  Value :: Int -> Expr
  Add :: Expr -> Expr -> Expr
  Mult :: Expr -> Expr -> Expr
  deriving instance Show

eval :: Expr -> Int
eval (Value i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mult e1 e2) = eval e1 * eval e2

sampleExpr :: Expr
sampleExpr = Add (Value 10) (Mult (Add (Value 20) (Value 10)) (Value 20))

-- >>> let a = sampleExpr
-- <interactive>:1317:2-11: error: Variable not in scope: sampleExpr

data Parser a where
  Return :: a -> Parser a
  Unparser :: (String -> [(a, String)]) -> (a -> Parser b) -> Parser b

instance Functor Parser where
  fmap f (Return x) = Return (f x)
