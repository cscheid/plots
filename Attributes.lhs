> module Attributes where

--------------------------------------------------------------------------------
Attributes extract "columns" from data points and have names

> newtype Attribute a b = MkAttribute ((a -> b), String)

> attributeFun :: Attribute a b -> (a -> b)
> attributeFun (MkAttribute (f, n)) = f
