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

> import Decorations
> import Geom
> import Iso
> import DiagramUtils
> import Scales
> import Datasets
> import Attributes
> import qualified ColorBrewer

--------------------------------------------------------------------------------
the scales

> xScale     rows attr = autoAffineScale   rows attr # slack 1.1
> yScale     rows attr = autoAffineScale   rows attr # slack 1.1
> sizeScale  rows attr = autoAffineScale   rows attr # rangeXform (Iso (\x -> x + 1.0) (\x -> x - 1.0))
> colorScale rows attr = autoDiscreteScale rows attr # rangeXform ColorBrewer.set1

> geomPoint = (GeomPoint
>              (Just (sepalLength, xScale))
>              (Just (petalLength, yScale))
>              (Just (sepalWidth, sizeScale))
>              (Just (species, colorScale)))
> thePlot  = splot      geomPoint iris
> grid     = background geomPoint iris
> legends' = legends    geomPoint iris

> test :: Show b => GeomPoint rowT b Double -> [rowT] -> DC
> test a b = ((splot <> background) # centerY ||| (const . const $ strutX 0.1) ||| (legends # centerY)) a b # (pad 1.2)

> main = do 
>        print $ intervalScaleDomain (sizeScale iris sepalWidth)
>        print $ intervalScaleRange (sizeScale iris sepalWidth)
>        defaultMain $ test geomPoint iris
