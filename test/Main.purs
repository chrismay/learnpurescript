module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Main(List(..),str)
import Test.Assert(assert,ASSERT)

main :: forall e. Eff (assert :: ASSERT | e) Unit
main = do
  assert (str(Cons 1 (Cons 2 Nil)) == "12" )
