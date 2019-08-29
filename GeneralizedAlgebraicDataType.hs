{- see https://en.wikipedia.org/wiki/Generalized_algebraic_data_type -}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

data Lam :: * -> * where
  Lift :: a -> Lam a -- ^ lifted value
  Pair :: Lam a -> Lam b -> Lam (a, b) -- ^ product
  Lam :: (Lam a -> Lam b) -> Lam (a -> b) -- ^ lambda abstraction
  App :: Lam (a -> b) -> Lam a -> Lam b -- ^ function application
  Fix :: Lam (a -> a) -> Lam a -- ^ fixed point
  LitI :: Int -> Lam Int
 --  IsZero ::(a -> Int) -> Lam a

eval :: Lam t -> t
eval (Lift v)   = v
eval (Pair l r) = (eval l, eval r)
eval (Lam f)    = \x -> eval (f (Lift x))
eval (App f x)  = (eval f) (eval x)
eval (Fix f)    = (eval f) (eval (Fix f))
eval (LitI n)   = n
-- eval (IsZero n) = (eval n

-- >>> eval(fact)(0)
-- 1

-- >>> eval(fact)(10)
-- 3628800
fact =
  Fix
    (Lam
       (\f ->
          Lam
            (\y ->
               Lift
                 (if eval y == 0
                    then 1
                    else eval y * (eval f) (eval y - 1)))))

-- Contracts
data Contract :: * -> * where
  Pred :: (a -> Bool) -> Contract a
  Fun :: Contract a -> Contract b -> Contract (a -> b)
  DFun :: Contract a -> (a -> Contract b) -> Contract (a -> b)

--(==>) :: Contract a -> Contract b -> Contract (a -> b)
assert :: Contract a -> a -> a
assert (Pred p) x =
  if p x
    then x
    else error "contract violation"
assert (Fun pre post) f = assert post . f . assert pre

--assert (DFun pre post) f = assert post . assert pre . f
pos :: (Num a, Ord a) => Contract a
pos = Pred (> 0)

-- >>> assert pos 2 == 2
-- True

-- >>> assert pos 0 == 1
-- *** Exception: contract violation
-- CallStack (from HasCallStack):
--   error, called at /var/folders/qj/l2fx79l139724lqywzmrjr540000gn/T/danterMGFu7.hs:50:10 in main:Main


true :: (Ord a) =>  Contract a
true = undefined
-- >>> assert true x == x
