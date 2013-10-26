> module Plots.Decorations where

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Backend.SVG
> import Diagrams.Coordinates
> import Diagrams.Prelude hiding (apply, view)
> import Graphics.SVGFonts.ReadFont
> import Diagrams.Backend.SVG.CmdLine
> import Data.Colour.SRGB.Linear
> import Data.Colour.SRGB
> import Diagrams.TwoD.Text
> import Numeric
> import Data.List
> import Data.Default

> import Plots.Legends
> import Plots.Geom
> import Plots.Iso
> import Plots.DiagramUtils
> import Plots.Scales
> import Plots.Datasets
> import Plots.Attributes
> import qualified Plots.ColorBrewer
> import Data.Maybe
> import Control.Lens hiding ((#), (&))
> import qualified Control.Lens as L

> background :: GeomPoint rowT b Double -> [rowT] -> DC
> background geom rows = 
>     backgroundGrid xscale yscale
>     where
>     Just px = view geomPointX geom
>     Just py = view geomPointY geom
>     xscale = scaleFromAffineScaleInContext px rows
>     yscale = scaleFromAffineScaleInContext py rows

> legends :: Show b => GeomPoint rowT b Double -> [rowT] -> DC
> legends geom rows =
>     foldr (===) mempty spacedLegs
>     where
>     psize =  view geomPointSize geom
>     pcolor = view geomPointColor geom
>     cLegs = map (discreteColorLegend . flip scaleFromDiscreteScaleInContext rows) (maybeToList pcolor)
>     sLegs = map (sizeScaleLegend'    . flip scaleFromAffineScaleInContext   rows) (maybeToList psize)
>     sizeScaleLegend' = sizeScaleLegend (circle 0.01)
>     allLegs = cLegs ++ sLegs
>     spacedLegs = intersperse (strutY 0.05) allLegs

--------------------------------------------------------------------------------

backgroundGrid draws a background grid for your typical scatterplot drawing.

> backgroundGrid :: IntervalScale Double Double -> IntervalScale Double Double -> DC
> backgroundGrid xScale yScale = v |||> ((hLines <> vLines <> bg) === h)
> -- backgroundGrid xScale yScale = (hLines <> vLines <> bg)
>     where xTitle = intervalScaleName xScale
>           yTitle = intervalScaleName yScale
>           bg = rect 1 1 # translate (r2 (0.5, 0.5))
>                         # fc (rgb 0.9 0.9 0.9)
>                         # lineColor transparent
>                         # centerXY
>           niceShow :: Double -> String
>           niceShow x = showFFloat (Just 2) x "" -- FIXME
>           alphaFromTick :: Double -> Double
>           alphaFromTick t
>               | abs (fromIntegral (floor t) - t) < 1e-5 = 1
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


