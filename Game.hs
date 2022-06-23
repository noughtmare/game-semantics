module Game where

import Data.Set (Set)
import qualified Data.Set as Set


data Arena a = MkArena
  { enabling :: a -> a -> Bool
  , initial :: Set a
  }

type Play a = [a]

data Nat = Z | S Nat

instance Num Nat where
  fromInteger 0 = Z
  fromInteger n
    | n > 0 = S (fromIntegral n)
    | otherwise = error "boom"

data NatMove = Q | N Nat

data Polarity = P | O

natArena :: Arena NatMove
natArena = MkArena natEnabling (Set.singleton Q) where
  natEnabling Q (N _) = True
  natEnabling _ _ = False
