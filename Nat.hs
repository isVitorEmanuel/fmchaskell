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
-- exponencial
(^) :: Nat -> Nat -> Nat
m ^ O = S O
m ^ S O = m
m ^ (S n) = m * (m ^ n)
-- dobro
double :: Nat -> Nat
double O = O
double (S m) = S (S (double m))
-- predecessor
pred :: Nat -> Nat
pred O = O
pred (S m) = m
-- factorial
fact :: Nat -> Nat
fact O = S O
fact (S m) = S m * fact m
