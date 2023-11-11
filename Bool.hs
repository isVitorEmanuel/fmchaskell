{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Bool where
    
import Prelude hiding 
    (Bool, True, False, not, (&&), (||), xou, if_then_else)
import Nat

data Bool = False | True
    deriving ( Eq , Show )

-- Negação
not :: Bool -> Bool
not True = False
not _ = True

-- Conjunção
(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

-- Disjunção
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

-- Disjunção Exclusiva
xou :: Bool -> Bool -> Bool
True `xou` True = False
False `xou` False = False
_ `xou` _ = True

-- Se_bla, então bli
if_then_else :: Bool -> Nat -> Nat -> Nat
if_then_else True n _ = n
if_then_else False _ n = n