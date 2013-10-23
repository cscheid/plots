> module Decorations where

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

> import Legends
> import Geom
> import Iso
> import DiagramUtils
> import Scales
> import Datasets
> import Attributes
> import qualified ColorBrewer

> background :: GeomPoint rowT b Double -> [rowT] -> DC
> background (GeomPoint px py _ _) rows = 
>     backgroundGrid xscale yscale
>     where
>     xscale = xyFromGeomSpec px rows
>     yscale = xyFromGeomSpec py rows

> legends :: Show b => GeomPoint rowT b Double -> [rowT] -> DC
> legends (GeomPoint px py psize pcolor) rows =
>     discreteColorLegend cscale === strutY 0.05 === sizeScaleLegend (circle 0.01) sscale
>     where
>     cscale = colorFromGeomSpec pcolor rows
>     sscale = xyFromGeomSpec psize rows
