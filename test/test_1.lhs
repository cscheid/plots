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

> import Plots.Draw
> import Plots.Decorations
> import Plots.Geom
> import Plots.Iso
> import Plots.DiagramUtils
> import Plots.Scales
> import Plots.Datasets
> import Plots.Attributes
> import qualified Plots.ColorBrewer
> import qualified Plots.Attributes as Attributes
> import Control.Lens hiding ((#))
> import qualified Control.Lens as L
> import Data.Maybe

--------------------------------------------------------------------------------
the scales

> type IrisRow = (Double, Double, Double, Double, String)

> sizeScale  rows attr = autoAffineScale   rows attr # rangeXform (Iso (\x -> x + 1.0) (\x -> x - 1.0))
> colorScale rows attr = autoDiscreteScale rows attr # rangeXform Plots.ColorBrewer.set1

> geomPoint1 :: GeomPoint IrisRow String Double
> geomPoint1 = geomPoint sepalLength petalLength

> geomPoint2 = geomPoint1 # withColorAttr species
> geomPoint3 = geomPoint1 # withSizeAttr sepalWidth
> geomPoint4 = geomPoint1 # withSizeAttr sepalWidth # withColorAttr species
> geomPoint5 = geomPoint1 # withXAttr petalWidth

> geomHLine1 = geomHLine (fromJust (L.view geomPointY geomPoint1))

> main = renderSVG "out.svg" (Height 600) ((hline geomHLine1 iris 3 <> draw geomPoint2 iris) # pad 1.1)

--------------------------------------------------------------------------------

> attrs = [sepalLength, sepalWidth, petalLength, petalWidth]

> stackH = foldr1 (|||)
> stackV = foldr1 (===)

> splom :: GeomPoint IrisRow String Double -> [Attributes.Attribute IrisRow Double] -> [IrisRow] -> DC
> splom geom attrs points = stackH (map stackV plots) 
>     where
>     rows = map (\s -> geom # withXAttr s) attrs
>     rowCols = map (\geom -> map (\s -> geom # withYAttr s) attrs) rows
>     plots = map (\col -> map (\geom -> draw geom points) col) rowCols

> sbs = foldr1 (|||) $ map (\t -> draw (geomPoint1 # withXAttr t)) attrs
