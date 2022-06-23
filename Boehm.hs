module Boehm where

import Common

data PCF
  = Var String
  | Abs String PCF PCF
  | App PCF PCF
  | Pred
  | Succ
  | IsZero
  | Case PCF [(Val, PCF)]
  | Y
  | B Bool
  | N Nat

data Val = VN Nat | VB Bool

newtype PCFBoehm = PCFBoehm M

data M = M [String] W
data W = W String [M] [(Val, W)] | Base Val

-- \x -> succ x
-- \x -> pred (succ (succ x))
