> module Geom where

> {-# LANGUAGE NoMonomorphismRestriction #-}

> import Diagrams.Coordinates
> import Diagrams.Prelude

> import Iso
> import DiagramUtils
> import Scales
> import Attributes

> data GeomPoint rowT b a = GeomPoint 
>     { pointGeomX     :: Maybe (Attributes.Attribute rowT Double,
>                                [rowT] -> Attributes.Attribute rowT Double -> AffineScale),
>       pointGeomY     :: Maybe (Attributes.Attribute rowT Double,
>                                [rowT] -> Attributes.Attribute rowT Double -> AffineScale),
>       pointGeomSize  :: Maybe (Attributes.Attribute rowT Double,
>                                [rowT] -> Attributes.Attribute rowT Double -> AffineScale),
>       pointGeomColor :: Maybe (Attributes.Attribute rowT b,
>                                [rowT] -> Attributes.Attribute rowT b -> DScale b (Colour a))
>     }

> splot :: GeomPoint rowT b Double -> [rowT] -> DC
> splot (GeomPoint px py psize pcolor) rows = -- attr xscale yattr yscale sattr sscale cattr cscale) rows =
>     scatterplot where
>      shapeFun rows x = circle 0.01 # fc (color x) # lc white # lw 0.002
>          where color = ap (colorFromGeomSpec pcolor rows) . ap (attrFromGeomSpec pcolor)

>      scatterplot     = (view (p2 (0,0)) (r2 (1,1)) $ mconcat $ zipWith translated sizedShapes points) # translate ((-0.5) & (-0.5))
>      x               = xyFromGeomSpec px rows
>      y               = xyFromGeomSpec py rows
>      size            = xyFromGeomSpec psize rows
>      xFun            = ap x    . ap (attrFromGeomSpec px)
>      yFun            = ap y    . ap (attrFromGeomSpec py)
>      sizeFun         = ap size . ap (attrFromGeomSpec psize)
>      sizes           = map sizeFun rows
>      shapes          = map (shapeFun rows) rows
>      sizedShapes     = zipWith (\x y -> x # scale y) shapes sizes
>      points          = map (\pt -> (xFun pt, yFun pt)) rows

> colorFromGeomSpec :: Maybe (Attributes.Attribute rowT b,
>                             [rowT] -> Attributes.Attribute rowT b -> DScale b (Colour a)) -> [rowT] -> DScale b (Colour a)
> colorFromGeomSpec Nothing _                    = error "diiie"
> colorFromGeomSpec (Just (attr, scale)) rows    = scale rows attr

> xyFromGeomSpec :: Maybe (Attributes.Attribute rowT Double,
>                          [rowT] -> Attributes.Attribute rowT Double -> AffineScale) -> [rowT] -> AffineScale
> xyFromGeomSpec    Nothing            _      = error "diiie"
> xyFromGeomSpec    (Just (attr, scale)) rows = scale rows attr

> attrFromGeomSpec Nothing          = error "diiie"
> attrFromGeomSpec (Just (attr, _)) = attr
