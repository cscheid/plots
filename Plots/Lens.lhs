> {-# LANGUAGE Rank2Types #-}
> {-# LANGUAGE TypeFamilies #-}
> module Plots.Lens where
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

> class HasY d where
>     type HasYTarget d
>     type HasYRowType d
>     y :: Lens' d (HasYTarget d)
>     withYAttr :: Attribute (HasYRowType d) Double -> d -> d
>     yScale :: d -> [HasYRowType d] -> Maybe AffineScale

> class HasData d where
>     type HasDataTarget d
>     type HasDataRowType d
>     data_ :: Lens' d (HasDataTarget d)

> withData :: (HasData d, HasDataTarget d ~ Maybe [HasDataRowType d]) => 
>             [HasDataRowType d] -> d -> d
> withData = set data_ . Just

--------------------------------------------------------------------------------

> withAttr lens scale = over lens . dAttr scale
>     where
>     dAttr d attr Nothing           = Just (attr, d)
>     dAttr _ attr (Just (_, scale)) = Just (attr, scale)
