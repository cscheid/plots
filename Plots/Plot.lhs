> {-# LANGUAGE TemplateHaskell #-}
> module Plots.Plot where

> import Plots.Geom
> import Plots.Scales
> import qualified Plots.Attributes as A
> import Control.Lens hiding ((#))
> import Control.Lens.TH

> data Plot rowT = Plot 
>     { _plotData :: Maybe [rowT],
>       _plotX    :: Maybe (AffineScaleInContext rowT),
>       _plotY    :: Maybe (AffineScaleInContext rowT)
>     }
>
> makeLenses ''Plot

> plot :: Plot rowT
> plot = Plot Nothing Nothing Nothing

--------------------------------------------------------------------------------

> instance HasX (Plot rowT) where
>     type HasXTarget (Plot rowT) = Maybe (AffineScaleInContext rowT)
>     type HasXRowType (Plot rowT) = rowT
>     x = plotX
>     withXAttr = withAttr x defaultXScaleInContext
>     xScale geom rows = case view x geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr

> instance HasY (Plot rowT) where
>     type HasYTarget (Plot rowT) = Maybe (AffineScaleInContext rowT)
>     type HasYRowType (Plot rowT) = rowT
>     y = plotY
>     withYAttr = withAttr plotY defaultYScaleInContext
>     yScale geom rows = case view y geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr


