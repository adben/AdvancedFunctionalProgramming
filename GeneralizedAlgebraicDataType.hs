{- see https://en.wikipedia.org/wiki/Generalized_algebraic_data_type -}
{-# LANGUAGE DataKinds, KindSignatures, GADTs, StandaloneDeriving, TypeFamilies #-}
data Lam :: * -> * where
  Lift :: a                     -> Lam a        -- ^ lifted value
  Pair :: Lam a -> Lam b        -> Lam (a, b)   -- ^ product
  Lam  :: (Lam a -> Lam b)      -> Lam (a -> b) -- ^ lambda abstraction
  App  :: Lam (a -> b) -> Lam a -> Lam b        -- ^ function application
  Fix  :: Lam (a -> a)          -> Lam a        -- ^ fixed point

eval :: Lam t -> t
eval (Lift v)   = v
eval (Pair l r) = (eval l, eval r)
eval (Lam f)    = \x -> eval (f (Lift x))
eval (App f x)  = (eval f) (eval x)
eval (Fix f)    = (eval f) (eval (Fix f))

fact = Fix (Lam (\f -> Lam (\y -> Lift (if eval y == 0 then 1 else eval y * (eval f) (eval y - 1)))))
--use eval(fact)(10)
