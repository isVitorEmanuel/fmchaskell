module Nat where
import Prelude 
  hiding ((+), (*), (^), quot, min, gcd, lcm, div, max, pred, rem)

data Nat = O | S Nat
  deriving (Eq , Show)

-- soma
(+) :: Nat -> Nat -> Nat
m + O = m
m + (S n) = S (m + n)
-- multiplicação
(*) :: Nat -> Nat -> Nat
m * O = O
m * (S n) = m + (m * n)