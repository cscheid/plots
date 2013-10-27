> {-# LANGUAGE Rank2Types #-}
> {-# LANGUAGE TypeFamilies #-}
> module Plots.Lens where

> import Plots.Plot
> import Plots.Geom
> import Control.Lens
> import Plots.Attributes
> import Plots.Scales

--------------------------------------------------------------------------------

> class HasX d where
>     type HasXTarget d
>     type HasXRowType d
>     x :: Lens' d (HasXTarget d)
>     withXAttr :: Attribute (HasXRowType d) Double -> d -> d
>     xScale :: d -> [HasXRowType d] -> Maybe AffineScale

> instance HasX (Plot rowT) where
>     type HasXTarget (Plot rowT) = Maybe (AffineScaleInContext rowT)
>     type HasXRowType (Plot rowT) = rowT
>     x = plotX
>     withXAttr = withAttr x defaultXScaleInContext
>     xScale geom rows = case view x geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr

> instance HasX (GeomPoint rowT b a) where
>     type HasXTarget (GeomPoint rowT b a) = Maybe (AffineScaleInContext rowT)
>     type HasXRowType (GeomPoint rowT b a) = rowT
>     x = geomPointX
>     withXAttr = withAttr x defaultXScaleInContext
>     xScale geom rows = case view x geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr

--------------------------------------------------------------------------------

> class HasY d where
>     type HasYTarget d
>     type HasYRowType d
>     y :: Lens' d (HasYTarget d)
>     withYAttr :: Attribute (HasYRowType d) Double -> d -> d
>     yScale :: d -> [HasYRowType d] -> Maybe AffineScale

> instance HasY (Plot rowT) where
>     type HasYTarget (Plot rowT) = Maybe (AffineScaleInContext rowT)
>     type HasYRowType (Plot rowT) = rowT
>     y = plotY
>     withYAttr = withAttr plotY defaultYScaleInContext
>     yScale geom rows = case view y geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr

> instance HasY (GeomPoint rowT b a) where
>     type HasYTarget (GeomPoint rowT b a) = Maybe (AffineScaleInContext rowT)
>     type HasYRowType (GeomPoint rowT b a) = rowT
>     y = geomPointY
>     withYAttr = withAttr geomPointY defaultYScaleInContext
>     yScale geom rows = case view y geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr

> instance HasY (GeomHLine rowT b a) where
>     type HasYTarget (GeomHLine rowT b a) = Maybe (AffineScaleInContext rowT)
>     type HasYRowType (GeomHLine rowT b a) = rowT
>     y = geomHLineY
>     withYAttr = withAttr geomHLineY defaultYScaleInContext
>     yScale geom rows = case view y geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr

--------------------------------------------------------------------------------

> withAttr lens scale = over lens . dAttr scale
>     where
>     dAttr d attr Nothing           = Just (attr, d)
>     dAttr _ attr (Just (_, scale)) = Just (attr, scale)
