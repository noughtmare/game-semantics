module Common where

data Nat = Z | S Nat

instance Num Nat where
  fromInteger 0 = Z
  fromInteger n
    | n > 0 = S (fromIntegral n)
    | otherwise = error "boom"

natToInt :: Nat -> Int
natToInt Z     = 0
natToInt (S n) = 1 + natToInt n

instance Eq Nat where
  Z == Z = True
  S n == S m = n == m

instance Ord Nat where
  x <= y = natToInt x <= natToInt y
