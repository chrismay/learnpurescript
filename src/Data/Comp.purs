module Data.Comp where

import Prelude
import Data.Array((..))
import Data.Foldable
import Data.String(length)
import Control.MonadZero (guard)


rep :: String-> Int->String
rep s n = foldl (\acc n->s<>acc) "" (1..n)

isLong:: String->Boolean
isLong s = length(s) > 2

repIsLong ::String->Int->Boolean
repIsLong  s n  = isLong $ rep s n

factorial :: Int->Int
factorial 1 = 1
factorial n = n * factorial (n-1)

-- *** 5.10

