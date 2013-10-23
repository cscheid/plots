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

--------------------------------------------------------------------------------

> background :: GeomPoint rowT b Double -> [rowT] -> DC
> background (GeomPoint xattr xscale yattr yscale _ _ _ _) rows = 
>     backgroundGrid (xscale rows xattr) (yscale rows yattr)

> legends :: Show b => GeomPoint rowT b Double -> [rowT] -> DC
> legends (GeomPoint xattr xscale yattr yscale sattr sscale cattr cscale) rows =
>     colorLegend (cscale rows cattr) === strutY 0.5 === sizeScaleLegend (circle 0.01) (sscale rows sattr)

--------------------------------------------------------------------------------

> thePlot  = splot      (GeomPoint sepalLength xScale petalLength yScale sepalWidth sizeScale species colorScale) iris
> grid     = background (GeomPoint sepalLength xScale petalLength yScale sepalWidth sizeScale species colorScale) iris
> legends' = legends    (GeomPoint sepalLength xScale petalLength yScale sepalWidth sizeScale species colorScale) iris

-- > test = (splot <> background) # centerY ||| (const . const $ strutX 0.1) ||| (legends' # centerY)

> test :: Show b => GeomPoint rowT b Double -> [rowT] -> DC
> test = ((splot <> background) # centerY ||| (const . const $ strutX 0.1) ||| (legends # centerY)) # pad 1.2
