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
> import Data.Colour.SRGB
> import Diagrams.TwoD.Text
> import Numeric
> import Data.List
> import Data.Default

> import Iso
> import DiagramUtils
> import Scales
> import Datasets
> import Attributes
> import qualified ColorBrewer

--------------------------------------------------------------------------------
scatterplot

> scatterplot :: IntervalScale Double Double -> 
>                IntervalScale Double Double -> 
>                IntervalScale Double Double -> 
>                (a -> DC) -> 
>                Attributes.Attribute a Double -> 
>                Attributes.Attribute a Double -> 
>                Attributes.Attribute a Double -> [a] -> DC
> scatterplot xScale yScale sizeScale shapeFun xAttr yAttr sizeAttr lst = 
>     (view (p2 (0,0)) (r2 (1,1)) $ mconcat $ zipWith translated sizedShapes points) # translate ((-0.5) & (-0.5))
>         where 
>     xFun    = attributeFun xAttr
>     yFun    = attributeFun yAttr
>     sizeFun = attributeFun sizeAttr
>     sizes = map (intervalScaleApply sizeScale . sizeFun) lst
>     shapes = map shapeFun lst
>     sizedShapes = zipWith (\x y -> x # scale y) shapes sizes
>     points = map (\pt -> (intervalScaleApply xScale $ xFun pt, intervalScaleApply yScale $ yFun pt)) lst

--------------------------------------------------------------------------------
le plot

> shapeFun x = circle 0.01 # fc color # lc white # lw 0.002
>     where color = cScaleFun colorScale $ attributeFun species x

> autoAffineScale' = autoAffineScale iris
> autoDiscreteScale' = autoDiscreteScale iris

> xScale     = autoAffineScale' sepalLength # slack 1.1
> yScale     = autoAffineScale' petalLength # slack 1.1
> sizeScale  = autoAffineScale' sepalWidth # intervalScaleRangeTransformation (Iso (\x -> x + 1.0) (\x -> x - 1.0))
> colorScale = autoDiscreteScale' species ColorBrewer.set1

> plot = scatterplot xScale yScale sizeScale shapeFun sepalLength petalLength sepalWidth iris
> grid = backgroundGrid xScale yScale

> legends = colorLegend colorScale === strutY 0.05 === sizeScaleLegend (circle 0.01) sizeScale

> main = do 
>        print $ intervalScaleDomain sizeScale
>        print $ intervalScaleRange sizeScale
>        defaultMain $ ((plot <> grid) # centerY ||| strutX 0.1 ||| (legends # centerY)) # pad 1.2

