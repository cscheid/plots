> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Backend.SVG
> import Diagrams.Coordinates
> import Diagrams.Prelude
> import Graphics.SVGFonts.ReadFont
> import Diagrams.Backend.SVG.CmdLine
> import Data.Colour.SRGB.Linear
> import Diagrams.TwoD.Text
> import Numeric
> import Data.List

> type DC = Diagram SVG R2

> translated :: DC -> (Double, Double) -> DC
> translated f (x, y) = f # translate (r2 (x, y))

--------------------------------------------------------------------------------
Scales

> data Scale = Scale { scaleDomain :: (Double, Double),
>                      scaleRange :: (Double, Double),
>                      scaleFun :: Double -> Double }

> baseScale :: Scale
> baseScale = Scale { scaleDomain = (0.0, 1.0),
>                     scaleRange = (0.0, 1.0),
>                     scaleFun = id }

> linearScale :: (Double, Double) -> (Double, Double) -> Scale
> linearScale (from1, from2) (to1, to2) = 
>     Scale { scaleDomain = domain,
>             scaleRange = range,
>             scaleFun = m }
>     where
>     minFrom = min from1 from2
>     maxFrom = max from1 from2
>     minTo = min to1 to2
>     maxTo = max to1 to2
>     domain = (minFrom, maxFrom)
>     range = (minTo, maxTo)
>     m v = (v - minFrom) / (maxFrom - minFrom) * (maxTo - minTo) + minTo

> autoScale :: [a] -> (a -> Double) -> Scale
> autoScale rows selector = linearScale (mn, mx) (0, 1)
>     where vs = map selector rows
>           mn = foldr1 min vs
>           mx = foldr1 max vs

> domainTransformation :: (Double -> Double) -> Scale -> Scale
> domainTransformation f scale = scale { scaleDomain = new,
>                                         scaleFun = scaleFun scale . f }
>     where (minFrom, maxFrom) = scaleDomain scale
>           new1 = f minFrom
>           new2 = f maxFrom
>           newMin = min new1 new2
>           newMax = max new1 new2
>           new = (newMin, newMax)

> slack :: Double -> Scale -> Scale
> slack amount scale = scale { scaleDomain = new,
>                              scaleFun = scaleFun scale . (scaleFun $ linearScale new old) }
>     where
>     new = (newMin, newMax)
>     old@(minFrom, maxFrom) = scaleDomain scale
>     span = maxFrom - minFrom
>     newMin = minFrom - span * (amount - 1.0) / 2
>     newMax = maxFrom + span * (amount - 1.0) / 2


CScale stands for Categorical Scale

> data CScale a b = CScale { cScaleDomain :: [a],
>                            cScaleRange :: [b],
>                            cScaleFun :: a -> b }

> categoricalColormap :: Fractional a => [b] -> (b -> Colour a) -> CScale b (Colour a)
> categoricalColormap vals valFun =
>     CScale { cScaleDomain = vals,
>              cScaleRange = map valFun vals,
>              cScaleFun = valFun }

autoCategoricalColormap :: Ord b => (a -> b) -> (b -> Colour) -> [a] -> CScale b Colour
autoCategoricalColormap selector colorFun rows =
    where vals = map selector colorFun
          
    CScale { 

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

> backgroundGrid :: Scale -> Scale -> DC
> backgroundGrid xScale yScale = (vTickMarks <> xTickMarks <> vLines <> hLines <> bg)
>     where bg = rect 1 1 # translate (r2 (0.5, 0.5))
>                         # fc (rgb 0.9 0.9 0.9)
>                         # lineColor transparent
>           niceShow x = showFFloat (Just 2) x "" -- FIXME
>           vTicks = ticks (scaleDomain xScale) 10
>           hTicks = ticks (scaleDomain yScale) 10
>           vTickLocations = map (\d -> scaleFun xScale d) vTicks
>           hTickLocations = map (\d -> scaleFun yScale d) hTicks
>           vLines = mconcat $ map (\x -> (x & 0.0) ~~ (x & 1.0) # lc white # lw 0.01) vTickLocations
>           hLines = mconcat $ map (\y -> (0.0 & y) ~~ (1.0 & y) # lc white # lw 0.01) hTickLocations
>           vTickMarks = strutY 0.2 <> (mconcat $ zipWith (\location v -> text (niceShow v) # scale 0.04 # translate (r2 (location, (-0.05)))) vTickLocations vTicks)
>           xTickMarks = strutX 0.2 <> (mconcat $ zipWith (\location v -> text (niceShow v) # scale 0.04 # translate (r2 ((-0.05), location))) hTickLocations hTicks)

--------------------------------------------------------------------------------
scatterplot

> scatterplot :: (a -> DC) -> Scale -> Scale -> (a -> Double) -> (a -> Double) -> [a] -> DC
> scatterplot shapeFun xScale yScale xFun yFun lst = 
>     view (p2 (0,0)) (r2 (1,1)) $ mconcat $ zipWith translated shapes points
>         where 
>     shapes = map shapeFun lst
>     points = map (\pt -> (scaleFun xScale $ xFun pt, scaleFun yScale $ yFun pt)) lst

--------------------------------------------------------------------------------
main

--------------------------------------------------------------------------------
le plot

> shapeFun x = circle 0.01 # fc color # lineColor transparent
>     where color = cScaleFun speciesColor $ species x

> autoScale'  = autoScale iris
> xScale = slack 1.1 $ autoScale' sepalLength 
> yScale = slack 1.1 $ autoScale' petalLength
> plot = scatterplot shapeFun xScale yScale sepalLength petalLength iris
> grid = backgroundGrid xScale yScale

> main = defaultMain $ ((plot <> grid) ||| strutX 0.1 ||| colorLegend speciesColor) # pad 1.2

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
