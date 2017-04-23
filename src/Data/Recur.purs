module Data.Recur where

import Prelude
import Data.Array.Partial(head, tail)
import Data.Array(null,(..),length)
import Partial.Unsafe(unsafePartial)
import Control.MonadZero (guard)
import Data.Foldable

even :: Int->Boolean
even 0 = true
even 1 = false
even n = even (n-2)

oneEven:: Int->Int
oneEven i = if (even i)
               then 1
               else 0

numEven :: Array Int->Int
numEven arr =
  if null arr
     then 0
     else oneEven (unsafePartial head arr) + numEven(unsafePartial tail arr)

factors :: Int -> Array (Array Int)
factors n = do
    i <- 1 .. n
    j <- i .. n
    guard $ i * j == n
    pure [i, j]

prime :: Int->Boolean
prime n = length (factors n) == 1

prod :: Array Int->Array Int->Array (Array Int)
prod a b = do
  x <- a
  y <- b
  pure [x,y]

pyth :: Int->Int->Int->Boolean
pyth a b c = a*a + b*b == c*c

tpl :: Int->Array (Array Int)
tpl max = do
  i <- 1 .. max
  j <- i .. max
  k <- j .. max
  guard $ pyth i j k
  pure [i,j,k]

firstFactors :: Int->Array Int 
firstFactors 1 = []
firstFactors n = if  (length fac >1) 
  then unsafePartial head (unsafePartial tail (fac))
  else []
    where fac = factors n

--factorize :: Int->Array (Array Int)
--factorize 1 = [[1]]
--factorize n = do
--  i <- firstFactors n
--  j <- i..n
--  guard $ i * j == n
--  pure [i

allTrue :: Array Boolean -> Boolean
allTrue arr = foldl (&&) true arr

countMatch :: forall a. (a -> Boolean) -> Array a -> Int
countMatch p arr = foldl addIfMatch 0 arr
  where
    addIfMatch :: Int->a->Int
    addIfMatch acc i = acc + inc
      where
        inc  | p i   = 1
             | otherwise = 0

