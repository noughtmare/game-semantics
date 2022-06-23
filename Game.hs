{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Game where

import Data.Set (Set)
import qualified Data.Set as Set

import Common

data Ty = TyNat

-- | Point to questions asked
type Pointer = Nat

data Move ty = Q (Question ty) | Val (Value ty)
data Turn (ty :: Ty) = Turn (Move ty) (Move ty) Pointer
type Play (ty :: Ty) = [Turn ty]

isLegal :: Play ty -> Bool
isLegal = go 0
  where
    -- n is the amount of questions of opponent
    go n [] = True
    go n (Turn o _ ref : ts) = ref < n && go (if isQ o then S n else n) ts

isQ :: Move ty -> Bool
isQ (Q _) = True
isQ _     = False


class Arena (ty :: Ty) where

  type family Question ty :: *
  type family Value ty :: *

  enabling :: Move ty -> Move ty -> Bool
  initial  :: Set (Move ty)


instance Arena TyNat where
  type Question TyNat = ()
  type Value TyNat = Nat

  enabling (Q _) (Val _) = True
  enabling _ _ = False

  initial = Set.singleton (Q ())
