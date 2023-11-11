module Nat where

import Prelude hiding 
    (O, S, (+), (*), (^), (-), double, pred, fact, fib, quot, min, max, monus, gcd, lcm, div, rem, if_then_else)

data Nat = O | S Nat
    deriving (Eq , Show)

-- + (Soma)
(+) :: Nat -> Nat -> Nat
m + O = m
m + (S n) = S (m + n)

-- * (Multiplicação)
(*) :: Nat -> Nat -> Nat
_ * O = O
m * (S n) = m + (m * n)

-- ^ (Exponencial)
(^) :: Nat -> Nat -> Nat
_ ^ O = S O
m ^ S O = m
m ^ (S n) = m * (m ^ n)

-- (-) Subtração
(-) :: Nat -> Nat -> Nat
O - n = O
m - O = m
(S m) - (S n) = m - n

-- double (Dobro)
double :: Nat -> Nat
double O = O
double (S m) = S (S (double m))

-- pred (Predecessor)
pred :: Nat -> Nat
pred O = O
pred (S m) = m

-- fact (Fatorial)
fact :: Nat -> Nat
fact O = S O
fact (S m) = S m * fact m

-- fib (Fibonacci)
fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S (S n)) = fib (S n) + fib n

-- min (Mínimo)
min :: Nat -> Nat -> Nat
min (S m) (S n) = S (min m n)
min O _ = O
min _ O = O

-- max (Máximo)
max :: Nat -> Nat -> Nat
max O m = m
max m O = m
max (S m) (S n) = S (max m n)

-- monus (Uma Subtração Diferente)
monus :: Nat -> Nat -> Nat
monus m O = m
monus m (S n) = pred (monus m n)

-- div (Divisão) 
div :: Nat -> Nat -> (Nat, Nat)
div _ O = error "Zero is not divisor!"
div n m = 
    if leq n m then (O, n) 
    else (S n', m')
        where (n', m') = div (monus n m) m

-- quot (Quociente)
quot :: Nat -> Nat -> Nat
quot n m = fst(div n m)

-- rem (Resto da Divisão)
rem :: Nat -> Nat -> Nat
rem n m = snd(div n m)

-- gcd (Euclides!!!)
gcd :: Nat -> Nat -> Nat
gcd n O = n
gcd n m = gcd n (rem n m)

-- lcm (???)
-- Não sei qual seria essa função :(

-- leq (Menor que)
leq :: Nat -> Nat -> Bool
leq O _ = True
leq _ O = False
leq (S m) (S n) = leq m n

-- geq (Maior que)
geq :: Nat -> Nat -> Bool
geq O _ = False
geq _ O = True
geq (S m) (S n) = geq m n

-- ev (Par)
ev :: Nat -> Bool
ev O = True
ev (S m) = od m

-- od (Ímpar)
od :: Nat -> Bool
od O = False
od (S m) = ev m

-- isMul3 (Múltiplo de 3)
isMul3 :: Nat -> Bool
isMul3 O = True
isMul3 (S(S(S m))) = isMul3 m
isMul3 _ = False

-- isZero (É zero sksk)
isZero :: Nat -> Bool
isZero O = True
isZero _ = False