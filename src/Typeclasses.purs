module Typeclasses where
import Prelude
import Data.Array(cons)

newtype Complex = Complex
   { real :: Number
   , imaginary :: Number
   }

instance showComplex:: Show Complex where
  show (Complex c) = (show c.real) <> "+" <> (show c.imaginary) <>"i"
  
instance eqComplex :: Eq Complex where
  eq (Complex c1) (Complex c2) = c1.real == c2.real && c1.imaginary == c2.imaginary

data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: Eq a=> Eq (NonEmpty a) where
  eq (NonEmpty a1 arr1) (NonEmpty a2  arr2) = 
    a1 == a2 && arr1 == arr2

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty h1 t1) (NonEmpty h2 t2) = 
    NonEmpty h1 (t1 <> [h2] <> t2)

instance functorNonEmpty:: Functor NonEmpty  where
  map (f) (NonEmpty h t) = NonEmpty (f h) (map f t)

instance showNonEmpty:: Show a=>Show (NonEmpty a) where
  show (NonEmpty h t) = show ([h] <> t)

data Extended a = Finite a | Infinite

instance eqExtended :: (Eq a) => Eq (Extended a) where
  eq (Finite x) (Finite y) = x == y
  eq (Finite _) Infinite   = false
  eq Infinite (Finite _)   = false
  eq Infinite Infinite     = false

instance extendedOrd:: (Ord a)=>Ord (Extended a) where
  compare (Finite e1) (Finite e2)  = compare e1 e2
  compare Infinite Infinite = EQ
  compare Infinite (Finite _) = GT
  compare (Finite _) Infinite = LT
