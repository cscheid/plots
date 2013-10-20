--------------------------------------------------------------------------------
Scales

> module Scales where

> import Diagrams.Backend.SVG
> import Diagrams.Coordinates
> import Diagrams.Prelude hiding (apply)
> import Graphics.SVGFonts.ReadFont
> import Diagrams.Backend.SVG.CmdLine
> import Data.Colour.SRGB.Linear
> import Diagrams.TwoD.Text
> import Numeric
> import Data.List
> import Data.Default
> import Attributes

> import Iso
> import DiagramUtils

--------------------------------------------------------------------------------
Interval Scales

Interval scales are scales of the form r . d, where r and d are range
and domain isomorphisms; r are functions from [0, 1] -> b, and d are
functions from a -> [0, 1].

This includes the typical "linear scales" of scatterplot and histogram
axes, many "continuous color scales" of interest, (including
"non-linear" color scales in device space, by defining appropriate
curvilinear coordinates like HLS)

Interval scales try to capture the property that these scales are,
fundamentally, injective mappings from a closed interval to a set.

In addition, interval scales keep track of their domain and range
interval endpoints, and have names (for displaying purposes only)

> data IntervalScale a b = IntervalScale 
>     { intervalScaleDomainXform :: Iso a Double, 
>       intervalScaleRangeXform :: Iso Double b,
>       intervalScaleDomain :: (a, a),
>       intervalScaleRange :: (b, b),
>       intervalScaleName :: String
>     }

> fundamentalIntervalScale :: IntervalScale Double Double
> fundamentalIntervalScale = IntervalScale 
>     { intervalScaleDomainXform = Iso id id,
>       intervalScaleRangeXform = Iso id id,
>       intervalScaleDomain = (0.0, 1.0),
>       intervalScaleRange = (0.0, 1.0),
>       intervalScaleName = "<unnamed>"
>     }

> intervalScaleDomainTransformation :: Iso a b -> IntervalScale b c -> IntervalScale a c
> intervalScaleDomainTransformation d_new s = s
>     { intervalScaleDomainXform = d_old `o` d_new,
>       intervalScaleDomain = (apply (inverse d_new) old_min, apply (inverse d_new) old_max)
>     } 
>     where
>     d_old = intervalScaleDomainXform s
>     (old_min, old_max) = intervalScaleDomain s

> intervalScaleRangeTransformation :: Iso b c -> IntervalScale a b -> IntervalScale a c
> intervalScaleRangeTransformation d_new s = s
>     { intervalScaleRangeXform = d_new `o` d_old,
>       intervalScaleRange = (apply d_new old_min, apply d_new old_max)
>     } 
>     where
>     d_old = intervalScaleRangeXform s
>     (old_min, old_max) = intervalScaleRange s

> intervalScaleRename :: String -> IntervalScale a b -> IntervalScale a b
> intervalScaleRename x scale = scale { intervalScaleName = x }

--------------------------------------------------------------------------------
Notice how these are just defining an "Iso a b" interface for IntervalScale

> intervalScaleApply :: IntervalScale a b -> a -> b
> intervalScaleApply scale a = apply (g `o` f) a
>     where g = intervalScaleRangeXform scale
>           f = intervalScaleDomainXform scale


intervalScaleInverse . intervalScaleInverse = id

> intervalScaleInverse :: IntervalScale a b -> IntervalScale b a
> intervalScaleInverse (IntervalScale d r bd br n) = 
>     IntervalScale (inverse r) (inverse d) br bd n

--------------------------------------------------------------------------------
-- FIXME these need better names

> linearScale :: (Double, Double) -> (Double, Double) -> IntervalScale Double Double
> linearScale (from1, from2) (to1, to2) =
>     intervalScaleDomainTransformation domainIso .
>     intervalScaleRangeTransformation rangeIso $ fundamentalIntervalScale
>     where domainIso = Iso from12ToZero1 zero1ToFrom12
>           rangeIso = Iso zero1ToTo12 to12ToZero1
>           xyToZero1 x y v = (v - x) / (y - x)
>           zero1ToXY x y v = v * (y - x) + x
>           from12ToZero1 = xyToZero1 from1 from2
>           zero1ToFrom12 = zero1ToXY from1 from2
>           zero1ToTo12 = zero1ToXY to1 to2
>           to12ToZero1 = xyToZero1 to1 to2

> autoScale :: [a] -> Attribute a Double -> IntervalScale Double Double
> autoScale rows (MkAttribute selector name) 
>     = linearScale (mn, mx) (0, 1) # intervalScaleRename name
>       where vs = map selector rows
>             mn = foldr1 min vs
>             mx = foldr1 max vs


> slack :: Double -> IntervalScale Double Double -> IntervalScale Double Double
> slack amount scale = intervalScaleDomainTransformation domainIso scale
>     where (old_min, old_max) = intervalScaleDomain scale
>           half_span = (old_max - old_min) / 2
>           center = (old_max + old_min) / 2
>           new_domain@(new_min, new_max) = (center - half_span * amount, center + half_span * amount)
>           domainIso = Iso ab ba
>           ab = intervalScaleApply ls
>           ba = intervalScaleApply (intervalScaleInverse ls)
>           ls = linearScale (new_min, new_max) (old_min, old_max)

> sizeScaleLegend :: DC -> IntervalScale Double Double -> DC
> sizeScaleLegend shape sizeScale = ((strutY 1.5 === alignedText 0 0 title) # scale 0.04) # alignL === (bounded_shapes ||| strutX 0.05 ||| tickMarks) # alignL
>     where title = intervalScaleName sizeScale
>           sTicks = ticks (intervalScaleDomain sizeScale) 5
>           sizes = map (intervalScaleApply sizeScale) sTicks
>           shapes = map (\s -> shape # lineColor transparent # fc black # scale s) $ sizes
>           all_phantoms = phantom $ mconcat shapes
>           bounded_shapes = foldr1 (===) . intersperse (strutY 0.01) . map (\s -> all_phantoms <> s) $ shapes
>           tickMarks = foldr1 (===) . intersperse (strutY 0.01) . map (\s -> all_phantoms <> (text (show s) # scale 0.04)) $ sTicks

--------------------------------------------------------------------------------
CScale stands for Categorical Scale

> data CScale a b = CScale { cScaleDomain :: [a],
>                            cScaleRange :: [b],
>                            cScaleFun :: a -> b,
>                            cScaleName :: String }

> categoricalColormap :: Fractional a => [b] -> (b -> Colour a) -> String -> CScale b (Colour a)
> categoricalColormap vals valFun name =
>     CScale { cScaleDomain = vals,
>              cScaleRange = map valFun vals,
>              cScaleFun = valFun,
>              cScaleName = name
>              }

> categoricalColormap' :: (Fractional a, Eq b) => [Colour a] -> Colour a -> [b] -> String -> CScale b (Colour a)
> categoricalColormap' colors noneColor keys name =
>     CScale { cScaleDomain = keys,
>              cScaleRange = colors,
>              cScaleFun = \color -> (case cLookup color of
>                                     Nothing -> noneColor
>                                     Just color -> color),
>              cScaleName = name
>              } where
>     cLookup = flip lookup $ zip keys colors

--------------------------------------------------------------------------------
ticks, legends, bah

> ticks :: (Double, Double) -> Double -> [Double]
> ticks d m = takeWhile (< b) [a, a+c ..] where (a, b, c) = tickRange d m

Choose ticks sensibly, algorithm stolen from d3

> tickRange :: (Double, Double) -> Double -> (Double, Double, Double)
> tickRange (mn, mx) m = 
>     (fromIntegral (ceiling (mn / step)) * step,
>      fromIntegral (floor (mx / step)) * step + (step / 2.0),
>      step)
>     where span = mx - mn
>           step' = 10.0 ** fromIntegral (floor (logBase 10 (span / m)))
>           err = (m / span) * step'
>           step = if err <= 0.15 then step' * 10 else
>                  if err <= 0.35 then step' * 5 else
>                  if err <= 0.75 then step' * 2 else step'

> colorLegend :: Show b => CScale b (Colour Double) -> DC
> colorLegend cscale = (strutY 1.5 === alignedText 0 0 title # alignL === (foldr1 (===) $ intersperse (strutY 0.2) (zipWith colorEntry vs cs)) # alignL) # scale 0.04 
>     where title = cScaleName cscale
>           vs = cScaleDomain cscale
>           cs = cScaleRange cscale
>           colorEntry name color = square 1 # fc color # lineColor transparent ||| (alignedText 0 0.5 (show name)) # translate (r2 (1, 0))

--------------------------------------------------------------------------------

> backgroundGrid :: IntervalScale Double Double -> IntervalScale Double Double -> DC
> backgroundGrid xScale yScale = v |||> ((hLines <> vLines <> bg) === h)
>     where xTitle = intervalScaleName xScale
>           yTitle = intervalScaleName yScale
>           bg = rect 1 1 # translate (r2 (0.5, 0.5))
>                         # fc (rgb 0.9 0.9 0.9)
>                         # lineColor transparent
>                         # centerXY
>           niceShow x = showFFloat (Just 2) x "" -- FIXME
>           alphaFromTick t 
>               | fromIntegral (floor t) == t = 1
>               | otherwise = (alphaFromTick (t * 10)) / 2
>           vTicks = ticks (intervalScaleDomain xScale) 10
>           hTicks = ticks (intervalScaleDomain yScale) 10
>           vTickLocations = map (\d -> intervalScaleApply xScale d) vTicks
>           hTickLocations = map (\d -> intervalScaleApply yScale d) hTicks
>           vLines = (mconcat $ zipWith (\x t -> (x & 0.0) ~~ (x & 1.0) # lineColor (white `withOpacity` alphaFromTick t) # lw 0.005) vTickLocations vTicks) # translate ((-0.5) & (-0.5))
>           hLines = (mconcat $ zipWith (\y t -> (0.0 & y) ~~ (1.0 & y) # lineColor (white `withOpacity` alphaFromTick t) # lw 0.005) hTickLocations hTicks) # translate ((-0.5) & (-0.5))
>           vTickMarks = (mconcat $ zipWith (\location v -> text (niceShow v) # scale 0.04 # translate (r2 (location, 0))) vTickLocations vTicks) # withEnvelope (rect 1 0.06 # translate (0.5 & 0.0) :: D R2)
>           hTickMarks = (mconcat $ zipWith (\location v -> alignedText 0 0.5 (niceShow v) # scale 0.04 # translate (r2 ((-0.05), location))) hTickLocations hTicks) # withEnvelope (rect 0.1 1 # translate (0.0 & 0.5) :: D R2)
>           xScaleTitle = text xTitle # scale 0.04 # centerX # withEnvelope (rect 1 0.04 :: D R2)
>           yScaleTitle = text yTitle # scale 0.04 # rotateBy (1/4) # withEnvelope (rect 0.06 1 :: D R2)
>           v = (yScaleTitle # centerY) ||| (hTickMarks # centerY)
>           h = (vTickMarks # centerX) === (xScaleTitle # centerX)

