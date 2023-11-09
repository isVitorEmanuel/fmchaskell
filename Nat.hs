module Nat where
import Prelude 
  hiding ((+), (*), (^), (-), double, pred, fact, fib, quot, min, max, gcd, lcm, div, rem)

data Nat = O | S Nat
  deriving (Eq , Show)

-- soma
(+) :: Nat -> Nat -> Nat
m + O = m
m + (S n) = S (m + n)
-- multiplicação
(*) :: Nat -> Nat -> Nat
_ * O = O
m * (S n) = m + (m * n)
-- exponencial
(^) :: Nat -> Nat -> Nat
_ ^ O = S O
m ^ S O = m
m ^ (S n) = m * (m ^ n)
-- subtraçao
(-) :: Nat -> Nat -> Nat
O - n = O
m - O = m
(S m) - (S n) = m - n
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
min (S m) (S n) = S (min m n)
min O _ = O
min _ O = O
-- máximo
max :: Nat -> Nat -> Nat
max O m = m
max m O = m
max (S m) (S n) = S (max m n)
-- div - Thanos definiou durante a aula
--div :: (Nat, Nat) -> (Nat, Nat)
--div (m, n)
  -- m < n     = (O, n)
  --otherwise = let (q', r') = div (m - n, n)
                -- in (S q', r')

-- quot
-- rem
-- gcd
-- lcm

