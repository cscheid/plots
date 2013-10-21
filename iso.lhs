= Isomorphisms

Disclaimer: I really wish I knew how to use the Iso class from
Control.Lens.Iso, or something that had polymorphic isomorphism
support.

> module Iso where

== Isomorphism typeclass

Isomorphism are represented by a simple typeclass satisfying the following
laws: 

    (ap h `o` ap g) `o` ap f = ap h `o` (ap g `o` ap f)
    toIso $ ap (inv f) `o` ap f = Iso id id

> class Isomorphism f where 
>     ap :: f a b -> a -> b
>     inv :: f a b -> f b a
>     o :: f b c -> f a b -> f a c

The canonical isomorphism is a pair of functions.

> data Iso a b = Iso (a -> b) (b -> a)

> instance Isomorphism Iso where
>     ap = apply
>     inv = inverse
>     o = com

> inverse :: Iso a b -> Iso b a
> inverse (Iso a b) = Iso b a

> apply :: Iso a b -> a -> b
> apply (Iso f _) a = f a

> com :: Iso b c -> Iso a b -> Iso a c
> (Iso bc cb) `com` (Iso ab ba) = Iso (bc . ab) (ba . cb)

There is a canonical mapping from any Isomorphism to the Iso datatype,
which strips out everything but the mappings.

> toIso :: Isomorphism f => f a b -> Iso a b
> toIso f = Iso (ap f) (ap . inv $ f)

