module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

data List  a = Nil | Cons a (List a)
str :: forall a.(Show a) => List a -> String
str Nil = ""
str (Cons i l) = show i <> str l


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
        log (str (Cons 1 (Cons 2 Nil)))

