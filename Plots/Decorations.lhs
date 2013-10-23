> module Plots.Decorations where

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

> import Plots.Legends
> import Plots.Geom
> import Plots.Iso
> import Plots.DiagramUtils
> import Plots.Scales
> import Plots.Datasets
> import Plots.Attributes
> import qualified Plots.ColorBrewer
> import Data.Maybe

> background :: GeomPoint rowT b Double -> [rowT] -> DC
> background (GeomPoint px py _ _) rows = 
>     backgroundGrid xscale yscale
>     where
>     xscale = xyFromGeomSpec px rows
>     yscale = xyFromGeomSpec py rows

> legends :: Show b => GeomPoint rowT b Double -> [rowT] -> DC
> legends (GeomPoint px py psize pcolor) rows =
>     foldr (===) mempty spacedLegs
>     where
>     cLegs = map (discreteColorLegend . flip colorScaleFromPointGeom rows) (maybeToList pcolor)
>     sLegs = map (sizeScaleLegend' . flip xyFromGeomSpec rows) (maybeToList psize)
>     sizeScaleLegend' = sizeScaleLegend (circle 0.01)
>     allLegs = cLegs ++ sLegs
>     spacedLegs = intersperse (strutY 0.05) allLegs

