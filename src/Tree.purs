module Tree where

import Prelude

data Tree a = Leaf | Branch (Tree a) a (Tree a)

