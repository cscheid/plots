> {-# LANGUAGE NoMonomorphismRestriction #-}

> module Plots.ColorBrewer(
>   set1, set2, set3
> ) where

> import Diagrams.Prelude
> import Data.Colour.SRGB
> import Iso

= Categorical colormaps

> set1, set2, set3 :: (Floating a, Ord a) => Iso Integer (Colour a)
> [set1, set2, set3] = map helper [set1', set2', set3']

> set1' = (["#ff7f00","#377eb8","#4daf4a","#e41a1c","#984ea3",
>           "#ffff33","#a65628","#f781bf","#999999"], black)

> set2' = (["#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854",
>           "#ffd92f","#e5c494","#b3b3b3"], black)

> set3' = (["#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3",
>           "#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd",
>           "#ccebc5","#ffed6f"], black)

--------------------------------------------------------------------------------

=== helper functions

> helper (lst2_rgb, def2) = Iso (f lst1 lst2 def2) (f lst2 lst1 def1)
>     where f = functionFromListPairs
>           lst1 = [1..]
>           def1 = 0
>           lst2 = map sRGB24read lst2_rgb

> functionFromListPairs :: Eq a => [a] -> [b] -> b -> a -> b
> functionFromListPairs keys values ifNotFound key =
>     case lookup key (zip keys values) of
>     Nothing -> ifNotFound
>     Just v  -> v

= Notice

This product includes color specifications and designs developed by
Cynthia Brewer (http://colorbrewer.org/).
