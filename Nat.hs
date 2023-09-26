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
-- fibonacci
fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S (S n)) = fib (S n) + fib n
-- mínimo
min :: Nat -> Nat -> Nat
min O m = O
min m O = O
min (S m) (S n) = S (min m n)
-- máximo
max :: Nat -> Nat -> Nat
max O m = m
max m O = m
max (S m) (S n) = S (max m n)
