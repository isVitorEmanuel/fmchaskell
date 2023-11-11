{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Bool where

import Prelude hiding (True, False, Bool, (&&), (||), xou, if_then_else)

data Bool = True | False
    deriving (Eq, Show)
--conjunção
(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False
--disjunção
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True
--disjunção exclusiva
xou :: Bool -> Bool -> Bool
True `xou` True = False
False `xou` False = False
_ `xou` _ = True
--se_então-nat
if_then_else :: Bool -> a -> a -> a
if_then_else True n _ = n
if_then_else False _ m = m