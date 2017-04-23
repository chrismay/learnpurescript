module Test.Tree where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Tree(Tree(..))
import Test.Assert(assert,ASSERT)

main :: forall e. Eff (assert :: ASSERT | e) Unit
main = do
  assert (true == false )
