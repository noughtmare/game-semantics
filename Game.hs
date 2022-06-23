module Game where

import Data.Set (Set)
import qualified Data.Set as Set

import Common


data Arena a = MkArena
  { enabling :: a -> a -> Bool
  , initial  :: Set a
  }

-- | Point to questions asked
type Pointer = Nat

data Turn q v = Turn (Move q v) (Move q v) Pointer

type Play q v = [Turn q v]

isLegal :: Play q v -> Bool
isLegal = go 0
  where
    -- n is the amount of questions of opponent
    go n [] = True
    go n (Turn o _ ref : ts) = ref < n && go (if isQ o then S n else n) ts


data Lang
  = Nat Nat


data Move q v = Q q | Val v

isQ :: Move q v -> Bool
isQ (Q _) = True
isQ _     = False

data Polarity = P | O


-- | There is only one question (), values are Nats
type NatMove = Move () Nat

natArena :: Arena NatMove
natArena = MkArena natEnabling (Set.singleton (Q ())) where
  natEnabling (Q _) (Val _) = True
  natEnabling _ _ = False
