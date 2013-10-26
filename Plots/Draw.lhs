> module Plots.Draw where

> import Plots.Geom
> import Diagrams.Prelude
> import Plots.Decorations
> import Plots.Legends
> import Plots.Scales
> import Plots.DiagramUtils

> draw :: Show b => GeomPoint rowT b Double -> [rowT] -> DC
> draw a b = ((splot <> background) # centerY ||| (\ _ _ -> strutX 0.1) ||| (legends # centerY)) a b # pad 1.1

