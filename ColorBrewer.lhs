-- This product includes color specfications and designs developed by Cynthia Brewer (http://colorbrewer.org/).

> {-# LANGUAGE NoMonomorphismRestriction #-}

> module ColorBrewer where

> import Data.Colour.SRGB
> import Diagrams.Prelude
> import Scales
> import Attributes

Categorical colormaps

-- > set1, set2, set3 :: (Eq b, Floating a, Ord a) => [b] -> Attributes.Attribute a b -> CScale b (Colour a)
-- > set1 = autoCategoricalScale (map sRGB24read set1RGBs) black
-- > set2 = autoCategoricalScale (map sRGB24read set2RGBs) black
-- > set3 = autoCategoricalScale (map sRGB24read set3RGBs) black

> set1 = (map sRGB24read ["#ff7f00","#377eb8","#4daf4a","#e41a1c","#984ea3",
>                         "#ffff33","#a65628","#f781bf","#999999"], black)

> set2 = (map sRGB24read ["#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854",
>                         "#ffd92f","#e5c494","#b3b3b3"], black)

> set3 = (map sRGB24read ["#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3",
>                         "#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd",
>                         "#ccebc5","#ffed6f"], black)
