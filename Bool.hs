module Bool where

import Prelude hiding (True, False, Bool)

data Bool = True | False
    deriving (Eq, Show)

