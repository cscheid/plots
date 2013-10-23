= Isomorphisms

Disclaimer: I really wish I knew how to use the Iso class from
Control.Lens.Iso, or something that had polymorphic isomorphism
support.

> module Iso where

== Function typeclass

A Function is an abstract object that behaves as a function.

> class Function f where
>     ap :: f a b -> a -> b
>     o :: f b c -> f a b -> f a c

=== Why not a category?

If you're wondering why the above doesn't read like:

    instance Category f => Function f where
         ap :: f a b -> a -> b

Mainly, because a category requires me to define an identity mapping,
which is something I don't always want. Specifically, there's no generic
IntervalScale a a that looks like an identity mapping, for all a.

This is annoying, but I don't know how to fix it.

Functions should be Functions. Duh.

> instance Function (->) where
>     ap = id
>     o = (.)

== Isomorphism typeclass

An Isomorphism is a Function satisfying the following additional
laws:

    toIso $ (inv f) `o` f = Iso id id
    toIso $ f `o` (inv f) = Iso id id

In other words, once we have observed an isomorphism f, composing it
with its inverse needs to be the identity, as witnessed by the canonical
isomorphism class Iso.

> class Function f => Isomorphism f where 
>     inv :: f a b -> f b a

=== Iso

The canonical isomorphism is a pair of functions.

> data Iso a b = Iso (a -> b) (b -> a)

> instance Function Iso where
>     ap = apply
>     o = com
> instance Isomorphism Iso where
>     inv = inverse

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

