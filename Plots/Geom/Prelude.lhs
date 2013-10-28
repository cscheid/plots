> module Plots.Geom.Prelude (
>  defaultXScaleInContext,
>  defaultYScaleInContext,
>  defaultSizeScaleInContext,
>  defaultColorScaleInContext,
>  dAttr
> ) where

> import Data.Default
> import Diagrams.Prelude

> import qualified Plots.Attributes as A
> import qualified Plots.ColorBrewer
> import Plots.DiagramUtils
> import Plots.Scales

> dAttr d attr Nothing           = Just (attr, d)
> dAttr _ attr (Just (_, scale)) = Just (attr, scale)

> defaultYScaleInContext               = defaultXScaleInContext
> defaultXScaleInContext     rows attr = autoAffineScale   rows attr # slack 1.1
> defaultSizeScaleInContext  rows attr = autoAffineScale   rows attr

> defaultColorScaleInContext :: (Default b, Ord b, Ord a, Floating a) => [rowT] -> A.Attribute rowT b -> DScale b (Colour a)
> defaultColorScaleInContext rows attr = autoDiscreteScale rows attr # rangeXform Plots.ColorBrewer.set1
