module Eq where

import Syntax
import Iso

instance Eq Program where 
  a == b = a === b

instance Eq Formula where 
  a == b = a === b

instance Eq Expr where 
  a == b = a === b

instance Eq Term where 
  a == b = a === b

instance Eq Factor where 
  a == b = a === b
