notes

- Categorical colormap needs different legend than continuous
  colormap, so colormaps need to know how to make their own legends

- color needs to be decoupled from shape, so that we can make size legends
  separately

- the decision on which legends to show needs to be made depending on which
  scales are not the default ones. This means we need a system of default
  scales.

> {-# LANGUAGE NoMonomorphismRestriction #-}
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

> type DC = Diagram SVG R2

> translated :: DC -> (Double, Double) -> DC
> translated f (x, y) = f # translate (r2 (x, y))

--------------------------------------------------------------------------------
Isomorphisms

I really wish I knew how to use the Iso class from Control.Lens.Iso,
or something that had polymorphic isomorphism support
 
> data Iso a b = Iso (a -> b) (b -> a)

> inverse :: Iso a b -> Iso b a
> inverse (Iso a b) = Iso b a

> apply :: Iso a b -> a -> b
> apply (Iso f _) a = f a

> o :: Iso b c -> Iso a b -> Iso a c
> (Iso bc cb) `o` (Iso ab ba) = Iso (bc . ab) (ba . cb)

--------------------------------------------------------------------------------
Scales

Interval Scales are scales of the form r . f . d, where r and d are
range and domain isomorphisms, and f is an affine injective function
f :: {x : 0 <= x <= 1} -> a.

This includes the typical "linear scales" of scatterplot and histogram axes,
many "continuous color scales" of interest, (including "non-linear" color scales
in device space, by defining appropriate curvilinear coordinates like HLS)

Interval Scales try to capture the property that these scales are, fundamentally,
injective mappings from a closed interval to a set.

> data IntervalScale a b = IntervalScale 
>     { intervalScaleDomainXform :: Iso a Double, 
>       intervalScaleRangeXform :: Iso Double b,
>       intervalScaleDomain :: (a, a),
>       intervalScaleRange :: (b, b)
>     }

> fundamentalIntervalScale :: IntervalScale Double Double
> fundamentalIntervalScale = IntervalScale 
>     { intervalScaleDomainXform = Iso id id,
>       intervalScaleRangeXform = Iso id id,
>       intervalScaleDomain = (0.0, 1.0),
>       intervalScaleRange = (0.0, 1.0)
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

--------------------------------------------------------------------------------
Notice how these are just defining an "Iso a b" interface for IntervalScale

> intervalScaleApply :: IntervalScale a b -> a -> b
> intervalScaleApply scale a = apply (g `o` f) a
>     where g = intervalScaleRangeXform scale
>           f = intervalScaleDomainXform scale


intervalScaleInverse . intervalScaleInverse = id

> intervalScaleInverse :: IntervalScale a b -> IntervalScale b a
> intervalScaleInverse (IntervalScale d r bd br) = 
>     IntervalScale (inverse r) (inverse d) br bd


--------------------------------------------------------------------------------

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

> autoScale :: [a] -> (a -> Double) -> IntervalScale Double Double
> autoScale rows selector = linearScale (mn, mx) (0, 1)
>     where vs = map selector rows
>           mn = foldr1 min vs
>           mx = foldr1 max vs


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
> sizeScaleLegend shape sizeScale = bounded_shapes ||| strutX 0.05 ||| tickMarks
>     where sTicks = ticks (intervalScaleDomain sizeScale) 5
>           sizes = map (intervalScaleApply sizeScale) sTicks
>           shapes = map (\s -> shape # lineColor transparent # fc black # scale s) $ sizes
>           all_phantoms = phantom $ mconcat shapes
>           bounded_shapes = foldr1 (===) . intersperse (strutY 0.01) . map (\s -> all_phantoms <> s) $ shapes
>           tickMarks = foldr1 (===) . intersperse (strutY 0.01) . map (\s -> all_phantoms <> (text (show s) # scale 0.04)) $ sTicks

--------------------------------------------------------------------------------

CScale stands for Categorical Scale

> data CScale a b = CScale { cScaleDomain :: [a],
>                            cScaleRange :: [b],
>                            cScaleFun :: a -> b }

> categoricalColormap :: Fractional a => [b] -> (b -> Colour a) -> CScale b (Colour a)
> categoricalColormap vals valFun =
>     CScale { cScaleDomain = vals,
>              cScaleRange = map valFun vals,
>              cScaleFun = valFun }

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
> colorLegend cscale = (foldr1 (===) $ intersperse (strutY 0.2) (zipWith colorEntry vs cs)) # scale 0.04
>     where vs = cScaleDomain cscale
>           cs = cScaleRange cscale
>           colorEntry name color = rect 1 1 # fc color # lineColor transparent ||| (alignedText 0 0.5 (show name)) # translate (r2 (1, 0))

--------------------------------------------------------------------------------

> backgroundGrid :: IntervalScale Double Double -> IntervalScale Double Double -> DC
> backgroundGrid xScale yScale = (vTickMarks <> xTickMarks <> vLines <> hLines <> bg)
>     where bg = rect 1 1 # translate (r2 (0.5, 0.5))
>                         # fc (rgb 0.9 0.9 0.9)
>                         # lineColor transparent
>           niceShow x = showFFloat (Just 2) x "" -- FIXME
>           vTicks = ticks (intervalScaleDomain xScale) 10
>           hTicks = ticks (intervalScaleDomain yScale) 10
>           vTickLocations = map (\d -> intervalScaleApply xScale d) vTicks
>           hTickLocations = map (\d -> intervalScaleApply yScale d) hTicks
>           vLines = mconcat $ map (\x -> (x & 0.0) ~~ (x & 1.0) # lc white # lw 0.01) vTickLocations
>           hLines = mconcat $ map (\y -> (0.0 & y) ~~ (1.0 & y) # lc white # lw 0.01) hTickLocations
>           vTickMarks = strutY 0.2 <> (mconcat $ zipWith (\location v -> text (niceShow v) # scale 0.04 # translate (r2 (location, (-0.05)))) vTickLocations vTicks)
>           xTickMarks = strutX 0.2 <> (mconcat $ zipWith (\location v -> text (niceShow v) # scale 0.04 # translate (r2 ((-0.05), location))) hTickLocations hTicks)

--------------------------------------------------------------------------------
scatterplot

> scatterplot :: IntervalScale Double Double -> 
>                IntervalScale Double Double -> 
>                IntervalScale Double Double -> 
>                (a -> DC) -> 
>                (a -> Double) -> (a -> Double) -> (a -> Double) -> [a] -> DC
> scatterplot xScale yScale sizeScale shapeFun xFun yFun sizeFun lst = 
>     view (p2 (0,0)) (r2 (1,1)) $ mconcat $ zipWith translated sizedShapes points
>         where 
>     sizes = map (intervalScaleApply sizeScale . sizeFun) lst
>     shapes = map shapeFun lst
>     sizedShapes = zipWith (\x y -> x # scale y) shapes sizes
>     points = map (\pt -> (intervalScaleApply xScale $ xFun pt, intervalScaleApply yScale $ yFun pt)) lst

--------------------------------------------------------------------------------
main

--------------------------------------------------------------------------------
le plot

> shapeFun x = circle 0.01 # fc color # lineColor transparent
>     where color = cScaleFun speciesColor $ species x

> autoScale' = autoScale iris
> xScale = slack 1.1 $ autoScale' sepalLength 
> yScale = slack 1.1 $ autoScale' petalLength
> sizeScale = intervalScaleRangeTransformation (Iso (\x -> x + 1.0) (\x -> x - 1.0)) $ autoScale' sepalWidth
> plot = scatterplot xScale yScale sizeScale shapeFun sepalLength petalLength sepalWidth iris
> grid = backgroundGrid xScale yScale

> legends = colorLegend speciesColor === strutY 0.05 === sizeScaleLegend (circle 0.01) sizeScale

> main = do 
>        print $ intervalScaleDomain sizeScale
>        print $ intervalScaleRange sizeScale
>        defaultMain $ ((plot <> grid) # centerY ||| strutX 0.1 ||| (legends # centerY)) # pad 1.2

-- >        defaultMain $ sizeScaleLegend (circle 0.01 # lineColor transparent # fc black) sizeScale


--------------------------------------------------------------------------------
die data

> sepalLength = \ (i,_,_,_,_) -> i
> sepalWidth  = \ (_,i,_,_,_) -> i
> petalLength = \ (_,_,i,_,_) -> i
> petalWidth  = \ (_,_,_,i,_) -> i
> species     = \ (_,_,_,_,i) -> i

> speciesColor = categoricalColormap
>                  ["setosa", "virginica", "versicolor"]
>                  (\c -> case c of
>                   "setosa" -> red
>                   "virginica" -> green
>                   "versicolor" -> blue
>                   _ -> white)

> iris = [(5.1,3.5,1.4,0.2,"setosa"),
>         (4.9,3.0,1.4,0.2,"setosa"),
>         (4.7,3.2,1.3,0.2,"setosa"),
>         (4.6,3.1,1.5,0.2,"setosa"),
>         (5.0,3.6,1.4,0.2,"setosa"),
>         (5.4,3.9,1.7,0.4,"setosa"),
>         (4.6,3.4,1.4,0.3,"setosa"),
>         (5.0,3.4,1.5,0.2,"setosa"),
>         (4.4,2.9,1.4,0.2,"setosa"),
>         (4.9,3.1,1.5,0.1,"setosa"),
>         (5.4,3.7,1.5,0.2,"setosa"),
>         (4.8,3.4,1.6,0.2,"setosa"),
>         (4.8,3.0,1.4,0.1,"setosa"),
>         (4.3,3.0,1.1,0.1,"setosa"),
>         (5.8,4.0,1.2,0.2,"setosa"),
>         (5.7,4.4,1.5,0.4,"setosa"),
>         (5.4,3.9,1.3,0.4,"setosa"),
>         (5.1,3.5,1.4,0.3,"setosa"),
>         (5.7,3.8,1.7,0.3,"setosa"),
>         (5.1,3.8,1.5,0.3,"setosa"),
>         (5.4,3.4,1.7,0.2,"setosa"),
>         (5.1,3.7,1.5,0.4,"setosa"),
>         (4.6,3.6,1.0,0.2,"setosa"),
>         (5.1,3.3,1.7,0.5,"setosa"),
>         (4.8,3.4,1.9,0.2,"setosa"),
>         (5.0,3.0,1.6,0.2,"setosa"),
>         (5.0,3.4,1.6,0.4,"setosa"),
>         (5.2,3.5,1.5,0.2,"setosa"),
>         (5.2,3.4,1.4,0.2,"setosa"),
>         (4.7,3.2,1.6,0.2,"setosa"),
>         (4.8,3.1,1.6,0.2,"setosa"),
>         (5.4,3.4,1.5,0.4,"setosa"),
>         (5.2,4.1,1.5,0.1,"setosa"),
>         (5.5,4.2,1.4,0.2,"setosa"),
>         (4.9,3.1,1.5,0.1,"setosa"),
>         (5.0,3.2,1.2,0.2,"setosa"),
>         (5.5,3.5,1.3,0.2,"setosa"),
>         (4.9,3.1,1.5,0.1,"setosa"),
>         (4.4,3.0,1.3,0.2,"setosa"),
>         (5.1,3.4,1.5,0.2,"setosa"),
>         (5.0,3.5,1.3,0.3,"setosa"),
>         (4.5,2.3,1.3,0.3,"setosa"),
>         (4.4,3.2,1.3,0.2,"setosa"),
>         (5.0,3.5,1.6,0.6,"setosa"),
>         (5.1,3.8,1.9,0.4,"setosa"),
>         (4.8,3.0,1.4,0.3,"setosa"),
>         (5.1,3.8,1.6,0.2,"setosa"),
>         (4.6,3.2,1.4,0.2,"setosa"),
>         (5.3,3.7,1.5,0.2,"setosa"),
>         (5.0,3.3,1.4,0.2,"setosa"),
>         (7.0,3.2,4.7,1.4,"versicolor"),
>         (6.4,3.2,4.5,1.5,"versicolor"),
>         (6.9,3.1,4.9,1.5,"versicolor"),
>         (5.5,2.3,4.0,1.3,"versicolor"),
>         (6.5,2.8,4.6,1.5,"versicolor"),
>         (5.7,2.8,4.5,1.3,"versicolor"),
>         (6.3,3.3,4.7,1.6,"versicolor"),
>         (4.9,2.4,3.3,1.0,"versicolor"),
>         (6.6,2.9,4.6,1.3,"versicolor"),
>         (5.2,2.7,3.9,1.4,"versicolor"),
>         (5.0,2.0,3.5,1.0,"versicolor"),
>         (5.9,3.0,4.2,1.5,"versicolor"),
>         (6.0,2.2,4.0,1.0,"versicolor"),
>         (6.1,2.9,4.7,1.4,"versicolor"),
>         (5.6,2.9,3.6,1.3,"versicolor"),
>         (6.7,3.1,4.4,1.4,"versicolor"),
>         (5.6,3.0,4.5,1.5,"versicolor"),
>         (5.8,2.7,4.1,1.0,"versicolor"),
>         (6.2,2.2,4.5,1.5,"versicolor"),
>         (5.6,2.5,3.9,1.1,"versicolor"),
>         (5.9,3.2,4.8,1.8,"versicolor"),
>         (6.1,2.8,4.0,1.3,"versicolor"),
>         (6.3,2.5,4.9,1.5,"versicolor"),
>         (6.1,2.8,4.7,1.2,"versicolor"),
>         (6.4,2.9,4.3,1.3,"versicolor"),
>         (6.6,3.0,4.4,1.4,"versicolor"),
>         (6.8,2.8,4.8,1.4,"versicolor"),
>         (6.7,3.0,5.0,1.7,"versicolor"),
>         (6.0,2.9,4.5,1.5,"versicolor"),
>         (5.7,2.6,3.5,1.0,"versicolor"),
>         (5.5,2.4,3.8,1.1,"versicolor"),
>         (5.5,2.4,3.7,1.0,"versicolor"),
>         (5.8,2.7,3.9,1.2,"versicolor"),
>         (6.0,2.7,5.1,1.6,"versicolor"),
>         (5.4,3.0,4.5,1.5,"versicolor"),
>         (6.0,3.4,4.5,1.6,"versicolor"),
>         (6.7,3.1,4.7,1.5,"versicolor"),
>         (6.3,2.3,4.4,1.3,"versicolor"),
>         (5.6,3.0,4.1,1.3,"versicolor"),
>         (5.5,2.5,4.0,1.3,"versicolor"),
>         (5.5,2.6,4.4,1.2,"versicolor"),
>         (6.1,3.0,4.6,1.4,"versicolor"),
>         (5.8,2.6,4.0,1.2,"versicolor"),
>         (5.0,2.3,3.3,1.0,"versicolor"),
>         (5.6,2.7,4.2,1.3,"versicolor"),
>         (5.7,3.0,4.2,1.2,"versicolor"),
>         (5.7,2.9,4.2,1.3,"versicolor"),
>         (6.2,2.9,4.3,1.3,"versicolor"),
>         (5.1,2.5,3.0,1.1,"versicolor"),
>         (5.7,2.8,4.1,1.3,"versicolor"),
>         (6.3,3.3,6.0,2.5,"virginica"),
>         (5.8,2.7,5.1,1.9,"virginica"),
>         (7.1,3.0,5.9,2.1,"virginica"),
>         (6.3,2.9,5.6,1.8,"virginica"),
>         (6.5,3.0,5.8,2.2,"virginica"),
>         (7.6,3.0,6.6,2.1,"virginica"),
>         (4.9,2.5,4.5,1.7,"virginica"),
>         (7.3,2.9,6.3,1.8,"virginica"),
>         (6.7,2.5,5.8,1.8,"virginica"),
>         (7.2,3.6,6.1,2.5,"virginica"),
>         (6.5,3.2,5.1,2.0,"virginica"),
>         (6.4,2.7,5.3,1.9,"virginica"),
>         (6.8,3.0,5.5,2.1,"virginica"),
>         (5.7,2.5,5.0,2.0,"virginica"),
>         (5.8,2.8,5.1,2.4,"virginica"),
>         (6.4,3.2,5.3,2.3,"virginica"),
>         (6.5,3.0,5.5,1.8,"virginica"),
>         (7.7,3.8,6.7,2.2,"virginica"),
>         (7.7,2.6,6.9,2.3,"virginica"),
>         (6.0,2.2,5.0,1.5,"virginica"),
>         (6.9,3.2,5.7,2.3,"virginica"),
>         (5.6,2.8,4.9,2.0,"virginica"),
>         (7.7,2.8,6.7,2.0,"virginica"),
>         (6.3,2.7,4.9,1.8,"virginica"),
>         (6.7,3.3,5.7,2.1,"virginica"),
>         (7.2,3.2,6.0,1.8,"virginica"),
>         (6.2,2.8,4.8,1.8,"virginica"),
>         (6.1,3.0,4.9,1.8,"virginica"),
>         (6.4,2.8,5.6,2.1,"virginica"),
>         (7.2,3.0,5.8,1.6,"virginica"),
>         (7.4,2.8,6.1,1.9,"virginica"),
>         (7.9,3.8,6.4,2.0,"virginica"),
>         (6.4,2.8,5.6,2.2,"virginica"),
>         (6.3,2.8,5.1,1.5,"virginica"),
>         (6.1,2.6,5.6,1.4,"virginica"),
>         (7.7,3.0,6.1,2.3,"virginica"),
>         (6.3,3.4,5.6,2.4,"virginica"),
>         (6.4,3.1,5.5,1.8,"virginica"),
>         (6.0,3.0,4.8,1.8,"virginica"),
>         (6.9,3.1,5.4,2.1,"virginica"),
>         (6.7,3.1,5.6,2.4,"virginica"),
>         (6.9,3.1,5.1,2.3,"virginica"),
>         (5.8,2.7,5.1,1.9,"virginica"),
>         (6.8,3.2,5.9,2.3,"virginica"),
>         (6.7,3.3,5.7,2.5,"virginica"),
>         (6.7,3.0,5.2,2.3,"virginica"),
>         (6.3,2.5,5.0,1.9,"virginica"),
>         (6.5,3.0,5.2,2.0,"virginica"),
>         (6.2,3.4,5.4,2.3,"virginica"),
>         (5.9,3.0,5.1,1.8,"virginica") ]
