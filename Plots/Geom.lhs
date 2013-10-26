> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> module Plots.Geom where

> import Data.Default
> import Diagrams.Coordinates hiding ((&))
> import Diagrams.Prelude hiding ((#))

> import qualified Diagrams.Prelude as D
> import Plots.Iso
> import Plots.DiagramUtils
> import Plots.Scales
> import qualified Plots.Attributes as A
> import Plots.ColorBrewer
> import Control.Lens
> import Control.Lens.TH

> type DoubleSpec rowT = (A.Attribute rowT Double,
>                         [rowT] -> A.Attribute rowT Double -> AffineScale)

> type DiscreteColorSpec rowT b a = (A.Attribute rowT b,
>                                    [rowT] -> A.Attribute rowT b -> DScale b (Colour a))

> data GeomPoint rowT b a = GeomPoint 
>     { _geomPointX     :: Maybe (DoubleSpec rowT),
>       _geomPointY     :: Maybe (DoubleSpec rowT),
>       _geomPointSize  :: Maybe (DoubleSpec rowT),
>       _geomPointColor :: Maybe (A.Attribute rowT b,
>                                [rowT] -> A.Attribute rowT b -> DScale b (Colour a))
>     }
>
> makeLenses ''GeomPoint

> splot :: GeomPoint rowT b Double -> [rowT] -> DC
> splot (GeomPoint mpx mpy mpsize mpcolor) rows = scatterplot 
>     where
>      shapeFun rows x = circle 0.01 & fc (color x) & lc white & lw 0.002
>          where color = case mpcolor of 
>                        Nothing -> const black
>                        Just _ -> ap (colorScaleFromPointGeom pcolor rows) . ap (attrFromGeomSpec pcolor)
>      scatterplot     = (D.view (p2 (0,0)) (r2 (1,1)) $ mconcat $ zipWith translated sizedShapes points) & translate (r2 (-0.5, (-0.5)))
>      Just pcolor     = mpcolor
>      Just psize      = mpsize
>      Just px         = mpx
>      Just py         = mpy
>      x               = xyFromGeomSpec px rows
>      y               = xyFromGeomSpec py rows
>      size            = xyFromGeomSpec psize rows
>      xFun            = ap x    . ap (attrFromGeomSpec px)
>      yFun            = ap y    . ap (attrFromGeomSpec py)
>      sizeFun         = case mpsize of
>                        Nothing -> const 1
>                        Just _ -> ap size . ap (attrFromGeomSpec psize)
>      sizes           = map sizeFun rows
>      shapes          = map (shapeFun rows) rows
>      sizedShapes     = zipWith (\x y -> x & scale y) shapes sizes
>      points          = map (\pt -> (xFun pt, yFun pt)) rows

> colorScaleFromPointGeom :: (A.Attribute rowT b,
>                             [rowT] -> A.Attribute rowT b -> DScale b (Colour a)) -> [rowT] -> DScale b (Colour a)
> colorScaleFromPointGeom (attr, scale) rows    = scale rows attr

> xyFromGeomSpec :: (A.Attribute rowT Double,
>                    [rowT] -> A.Attribute rowT Double -> AffineScale) -> [rowT] -> AffineScale
> xyFromGeomSpec    (attr, scale) rows = scale rows attr

> attrFromGeomSpec (attr, _) = attr

--------------------------------------------------------------------------------

> withXAttr  :: A.Attribute rowT Double -> GeomPoint rowT b a -> GeomPoint rowT b a
> withXAttr  attr geom = geom { _geomPointX  = new_attr (_geomPointX geom) }
>     where
>     new_attr Nothing           = Just (attr, defaultXScaleInContext)
>     new_attr (Just (_, scale)) = Just (attr, scale)

> withYAttr  :: A.Attribute rowT Double -> GeomPoint rowT b a -> GeomPoint rowT b a
> withYAttr  attr geom = geom { _geomPointY  = new_attr (_geomPointY geom) }
>     where
>     new_attr Nothing           = Just (attr, defaultYScaleInContext)
>     new_attr (Just (_, scale)) = Just (attr, scale)

> withColorAttr :: (Default d, Ord d, Ord c, Floating c) => A.Attribute rowT d -> GeomPoint rowT d c -> GeomPoint rowT d c
> withColorAttr attr (GeomPoint x y s Nothing) = GeomPoint x y s (Just (attr, defaultColorScaleInContext))
> withColorAttr attr (GeomPoint x y s (Just (_, scale))) = GeomPoint x y s (Just (attr, scale))

> withSize, withX, withY :: DoubleSpec rowT -> GeomPoint rowT b a -> GeomPoint rowT b a
> withSize = set geomPointSize . Just
> withX    = set geomPointX    . Just
> withY    = set geomPointY    . Just

> withSizeAttr  = set (geomPointSize . _Just . _1)
> withSizeScale = set (geomPointSize . _Just . _2)

-- > withXAttr    = set (geomPointX . _Just . _1)
-- > withSizeAttr  :: A.Attribute rowT Double -> GeomPoint rowT b a -> GeomPoint rowT b a
-- > withSizeAttr  attr geom = geom { _geomPointSize  = new_attr (_geomPointSize geom) }
-- >     where
-- >     new_attr Nothing           = Just (attr, defaultSizeScaleInContext)
-- >     new_attr (Just (_, scale)) = Just (attr, scale)

> withColor :: DiscreteColorSpec rowT b a -> GeomPoint rowT d c -> GeomPoint rowT b a
> withColor = set geomPointColor . Just

> defaultYScaleInContext               = defaultXScaleInContext
> defaultXScaleInContext     rows attr = autoAffineScale   rows attr & slack 1.1
> defaultSizeScaleInContext  rows attr = autoAffineScale   rows attr

> defaultColorScaleInContext :: (Default b, Ord b, Ord a, Floating a) => [rowT] -> A.Attribute rowT b -> DScale b (Colour a)
> defaultColorScaleInContext rows attr = autoDiscreteScale rows attr & rangeXform Plots.ColorBrewer.set1

> geomPoint :: A.Attribute rowT Double -> A.Attribute rowT Double -> GeomPoint rowT b a
> geomPoint xattr yattr = GeomPoint x y Nothing Nothing
>     where
>     x = Just (xattr, defaultXScaleInContext)
>     y = Just (yattr, defaultXScaleInContext)

