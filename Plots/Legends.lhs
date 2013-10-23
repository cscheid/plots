> module Plots.Legends where

> import Diagrams.Prelude
> import Plots.DiagramUtils
> import Plots.Scales
> import Data.List
> import Plots.Iso

> discreteColorLegend :: Show b => DScale b (Colour Double) -> DC
> discreteColorLegend cscale = (strutY 1.5 === alignedText 0 0 title # alignL === (foldr1 (===) $ intersperse (strutY 0.2) (zipWith colorEntry vs cs)) # alignL) # scale 0.04 
>     where title = dScaleName cscale
>           vs = dScaleDomain cscale
>           cs = dScaleRange cscale
>           colorEntry name color = square 1 # fc color # lineColor transparent ||| (alignedText 0 0.5 (show name)) # translate (r2 (1, 0))

> sizeScaleLegend :: DC -> IntervalScale Double Double -> DC
> sizeScaleLegend shape sizeScale = ((strutY 1.5 === alignedText 0 0 title) # scale 0.04) # alignL === (bounded_shapes ||| strutX 0.05 ||| tickMarks) # alignL
>     where title = intervalScaleName sizeScale
>           sTicks = ticks (intervalScaleDomain sizeScale) 5
>           sizes = map (ap sizeScale) sTicks
>           shapes = map (\s -> shape # lineColor transparent # fc black # scale s) $ sizes
>           all_phantoms = phantom $ mconcat shapes
>           bounded_shapes = foldr1 (===) . intersperse (strutY 0.01) . map (\s -> all_phantoms <> s) $ shapes
>           tickMarks = foldr1 (===) . intersperse (strutY 0.01) . map (\s -> all_phantoms <> (text (show s) # scale 0.04)) $ sTicks
