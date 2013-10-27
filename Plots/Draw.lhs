> module Plots.Draw where

> import Diagrams.Prelude
> import Plots.Geom
> import Plots.Decorations
> import Plots.Legends
> import Plots.Scales
> import Plots.DiagramUtils

> draw :: Show b => GeomPoint rowT b Double -> [rowT] -> DC
> draw a b = (((splot <> background) ||| (\ _ _ -> strutX 0.1) ||| (legends # centerY)) a b) # pad 1.1
