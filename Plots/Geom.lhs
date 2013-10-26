> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> module Plots.Geom(
>  GeomPoint, AffineScaleInContext, DiscreteScaleInContext, 
>  geomPoint,
>  geomPointX, geomPointY, geomPointSize, geomPointColor,
>  splot,
>  withXAttr, withYAttr, withSizeAttr, withColorAttr,
>  withX, withY, withSize, withColor,
>  defaultXScaleInContext,
>  defaultYScaleInContext,
>  defaultSizeScaleInContext,
>  defaultColorScaleInContext,
>  scaleFromAffineScaleInContext,
>  scaleFromDiscreteScaleInContext
> ) where

> import Data.Default
> import Diagrams.Coordinates hiding ((&))
> import Diagrams.Prelude

> import qualified Diagrams.Prelude as D
> import qualified Plots.Attributes as A
> import Plots.Iso
> import Plots.DiagramUtils
> import Plots.Scales
> import Plots.ColorBrewer
> import Control.Lens hiding ((#))
> import Control.Lens.TH

> type AffineScaleInContext rowT = (A.Attribute rowT Double,
>                         [rowT] -> A.Attribute rowT Double -> AffineScale)

> type DiscreteScaleInContext rowT b a = (A.Attribute rowT b,
>                               [rowT] -> A.Attribute rowT b -> DScale b (Colour a))

> data GeomPoint rowT b a = GeomPoint
>     { _geomPointX     :: Maybe (AffineScaleInContext rowT),
>       _geomPointY     :: Maybe (AffineScaleInContext rowT),
>       _geomPointSize  :: Maybe (AffineScaleInContext rowT),
>       _geomPointColor :: Maybe (DiscreteScaleInContext rowT b a)
>     }
>
> makeLenses ''GeomPoint
>
> geomPoint :: A.Attribute rowT Double -> A.Attribute rowT Double -> GeomPoint rowT b a
> geomPoint xattr yattr = GeomPoint x y Nothing Nothing
>     where
>     x = Just (xattr, defaultXScaleInContext)
>     y = Just (yattr, defaultXScaleInContext)

FIXME make this into Maybe DC when mpx or mpy are Nothing

> splot :: GeomPoint rowT b Double -> [rowT] -> DC
> splot (GeomPoint mpx mpy mpsize mpcolor) rows = scatterplot 
>     where
>      shapeFun rows x = circle 0.01 # fc (color x) # lc white # lw 0.002
>          where color = case mpcolor of 
>                        Nothing -> const black
>                        Just _ -> ap (scaleFromDiscreteScaleInContext pcolor rows) . ap (fst pcolor)
>      scatterplot     = (D.view (p2 (0,0)) (r2 (1,1)) $ mconcat $ zipWith translated sizedShapes points) # translate (r2 (-0.5, (-0.5)))
>      Just pcolor     = mpcolor
>      Just psize      = mpsize
>      Just px         = mpx
>      Just py         = mpy
>      x               = scaleFromAffineScaleInContext px rows
>      y               = scaleFromAffineScaleInContext py rows
>      size            = scaleFromAffineScaleInContext psize rows
>      xFun            = ap x    . ap (fst px)
>      yFun            = ap y    . ap (fst py)
>      sizeFun         = case mpsize of
>                        Nothing -> const 1
>                        Just _ -> ap size . ap (fst psize)
>      sizes           = map sizeFun rows
>      shapes          = map (shapeFun rows) rows
>      sizedShapes     = zipWith (\x y -> x # scale y) shapes sizes
>      points          = map (\pt -> (xFun pt, yFun pt)) rows

> scaleFromDiscreteScaleInContext :: (A.Attribute rowT b,
>                                     [rowT] -> A.Attribute rowT b -> DScale b (Colour a)) -> [rowT] -> DScale b (Colour a)
> scaleFromDiscreteScaleInContext (attr, scale) rows    = scale rows attr

> scaleFromAffineScaleInContext :: (A.Attribute rowT Double,
>                                   [rowT] -> A.Attribute rowT Double -> AffineScale) -> [rowT] -> AffineScale
> scaleFromAffineScaleInContext    (attr, scale) rows = scale rows attr

--------------------------------------------------------------------------------

> dAttr Nothing defaultScale attr = Just (attr, defaultScale)
> dAttr (Just (_, scale)) _  attr = Just (attr, scale)

> withXAttr  :: A.Attribute rowT Double -> GeomPoint rowT b a -> GeomPoint rowT b a
> withXAttr  attr geom = geom { _geomPointX  = dAttr (_geomPointX geom) defaultXScaleInContext attr }

> withYAttr  :: A.Attribute rowT Double -> GeomPoint rowT b a -> GeomPoint rowT b a
> withYAttr  attr geom = geom { _geomPointY  = dAttr (_geomPointY geom) defaultYScaleInContext attr }

> withSizeAttr  :: A.Attribute rowT Double -> GeomPoint rowT b a -> GeomPoint rowT b a
> withSizeAttr  attr geom = geom { _geomPointSize  = dAttr (_geomPointSize geom) defaultSizeScaleInContext attr }

> withColorAttr :: (Default d, Ord d, Ord c, Floating c) => A.Attribute rowT d -> GeomPoint rowT d c -> GeomPoint rowT d c
> withColorAttr attr geom = geom { _geomPointColor = dAttr (_geomPointColor geom) defaultColorScaleInContext attr }

> withSize, withX, withY :: AffineScaleInContext rowT -> GeomPoint rowT b a -> GeomPoint rowT b a
> withSize = set geomPointSize . Just
> withX    = set geomPointX    . Just
> withY    = set geomPointY    . Just

> withColor :: DiscreteScaleInContext rowT b a -> GeomPoint rowT d c -> GeomPoint rowT b a
> withColor = set geomPointColor . Just

> withSizeScale = set (geomPointSize . _Just . _2)

> defaultYScaleInContext               = defaultXScaleInContext
> defaultXScaleInContext     rows attr = autoAffineScale   rows attr # slack 1.1
> defaultSizeScaleInContext  rows attr = autoAffineScale   rows attr

> defaultColorScaleInContext :: (Default b, Ord b, Ord a, Floating a) => [rowT] -> A.Attribute rowT b -> DScale b (Colour a)
> defaultColorScaleInContext rows attr = autoDiscreteScale rows attr # rangeXform Plots.ColorBrewer.set1

