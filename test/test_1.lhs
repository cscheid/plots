> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Backend.SVG
> import Diagrams.Coordinates
> import Diagrams.Prelude hiding (apply)
> import Graphics.SVGFonts.ReadFont
> import Diagrams.Backend.SVG.CmdLine
> import Data.Colour.SRGB.Linear
> import Data.Colour.SRGB
> import Diagrams.TwoD.Text
> import Numeric
> import Data.List
> import Data.Default
> import Diagrams.TwoD.Size

> import Plots.Decorations
> import Plots.Geom
> import Plots.Iso
> import Plots.DiagramUtils
> import Plots.Scales
> import Plots.Datasets
> import Plots.Attributes
> import qualified Plots.ColorBrewer

--------------------------------------------------------------------------------
the scales

> xScale     rows attr = autoAffineScale   rows attr # slack 1.1
> yScale     rows attr = autoAffineScale   rows attr # slack 1.1
> sizeScale  rows attr = autoAffineScale   rows attr # rangeXform (Iso (\x -> x + 1.0) (\x -> x - 1.0))
> colorScale rows attr = autoDiscreteScale rows attr # rangeXform ColorBrewer.set1

> type IrisRow = (Double, Double, Double, Double, String)

> geomPoint :: GeomPoint IrisRow String Double
> geomPoint = (GeomPoint
>               (sepalLength, xScale)
>               (petalLength, yScale)
>               Nothing
>               Nothing)

> geomPoint2 :: GeomPoint IrisRow String Double
> geomPoint2 = (GeomPoint
>               (sepalLength, xScale)
>               (petalLength, yScale)
>               Nothing
>               (Just (species, colorScale)))

> geomPoint3 :: GeomPoint IrisRow String Double
> geomPoint3 = (GeomPoint
>               (sepalLength, xScale)
>               (petalLength, yScale)
>               (Just (sepalWidth, sizeScale))
>               Nothing)

> geomPoint4 :: GeomPoint IrisRow String Double
> geomPoint4 = (GeomPoint
>              (sepalLength, xScale)
>              (petalLength, yScale)
>              (Just (sepalWidth, sizeScale))
>              (Just (species, colorScale)))

> test :: Show b => GeomPoint rowT b Double -> [rowT] -> DC
> test a b = ((splot <> background) # centerY ||| (\ _ _ -> strutX 0.1) ||| (legends # centerY)) a b # pad 1.1

> main = do 
>        renderSVG "geomPoint.svg"  (Height 700) (test geomPoint iris) 
>        renderSVG "geomPoint2.svg" (Height 700) (test geomPoint2 iris)
>        renderSVG "geomPoint3.svg" (Height 700) (test geomPoint3 iris)
>        renderSVG "geomPoint4.svg" (Height 700) (test geomPoint4 iris)

