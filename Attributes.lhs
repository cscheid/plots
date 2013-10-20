> module Attributes where

--------------------------------------------------------------------------------
Attributes extract "columns" from data points and have names

> newtype Attribute a b = MkAttribute (a -> b) String
