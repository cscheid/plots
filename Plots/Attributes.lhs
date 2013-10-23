> module Plots.Attributes (
>     Attribute'(..),
>     Attribute(..)
> ) where

> import Data.Default
> import Plots.Iso

--------------------------------------------------------------------------------
= Attributes

Attributes extract "columns" from data points and have names

> newtype Attribute' s a b = MkAttribute ((a -> b), s)

> type Attribute a b = Attribute' String a b 

== Attributes have products

> attrPair :: Attribute' s a b -> Attribute' s' a c  -> Attribute' (s, s') a (b, c) 
> attrPair (MkAttribute (f1, n1)) (MkAttribute (f2, n2))  = MkAttribute (lift f1 f2, (n1, n2))
>     where lift f1 f2 a = (f1 a, f2 a)

> attrFst :: Attribute' (s, s') a (b, c) -> Attribute' s a b
> attrFst (MkAttribute (f, n)) = MkAttribute (fst . f, fst n)

> attrSnd :: Attribute' (s, s') a (b, c) -> Attribute' s' a c
> attrSnd (MkAttribute (f, n)) = MkAttribute (snd . f, snd n)

== Attributes are fundamentally functions

> attributeFun :: Attribute' s a b -> (a -> b)
> attributeFun (MkAttribute (f, n)) = f

> instance (Default d, Default s) => Default (Attribute' s a d) where
>     def = MkAttribute (const def, def)

This is unfortunate, the naming gets lost.

> instance Default s => Function (Attribute' s) where
>     ap (MkAttribute (f, _)) = f
>     MkAttribute (g, s2) `o` MkAttribute (f, s1) = MkAttribute (g . f, s1)

