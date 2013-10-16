--------------------------------------------------------------------------------
Isomorphisms

I really wish I knew how to use the Iso class from Control.Lens.Iso,
or something that had polymorphic isomorphism support
 
> module Iso where

> data Iso a b = Iso (a -> b) (b -> a)

> inverse :: Iso a b -> Iso b a
> inverse (Iso a b) = Iso b a

> apply :: Iso a b -> a -> b
> apply (Iso f _) a = f a

> o :: Iso b c -> Iso a b -> Iso a c
> (Iso bc cb) `o` (Iso ab ba) = Iso (bc . ab) (ba . cb)
