> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE TypeFamilies #-}

> module Plots.Geom(
>
>  GeomPoint, geomPoint, -- default constructor
>  geomPointX, geomPointY, geomPointSize, geomPointColor, -- lenses
>  AffineScaleInContext, DiscreteScaleInContext, -- why are these here? FIXME
>  splot,
>  withSizeAttr, withColorAttr,
>  defaultXScaleInContext,
>  defaultYScaleInContext,
>  defaultSizeScaleInContext,
>  defaultColorScaleInContext,
>  scaleFromAffineScaleInContext,
>  scaleFromDiscreteScaleInContext,
>
>  GeomHLine, geomHLine, -- default constructor
>  geomHLineY, -- lenses
>  hline, 
>
> ) where

> import Data.Default
> import Diagrams.Coordinates hiding ((&))
> import Diagrams.Prelude hiding (over)

> import qualified Diagrams.Prelude as D
> import qualified Plots.Attributes as A
> import qualified Control.Lens as L
> import Plots.Iso
> import Plots.DiagramUtils
> import Plots.Scales
> import Plots.ColorBrewer
> import Plots.Lens
> import Control.Lens hiding ((#))
> import Control.Lens.TH

--------------------------------------------------------------------------------
GeomPoint

> data GeomPoint rowT b a = GeomPoint
>     { _geomPointX     :: Maybe (AffineScaleInContext rowT),
>       _geomPointY     :: Maybe (AffineScaleInContext rowT),
>       _geomPointSize  :: Maybe (AffineScaleInContext rowT),
>       _geomPointColor :: Maybe (DiscreteScaleInContext rowT b a)
>     }
>
> makeLenses ''GeomPoint

> geomPoint :: GeomPoint rowT b a
> geomPoint = GeomPoint Nothing Nothing Nothing Nothing

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
>      xFun            = ap x . ap (fst px)
>      yFun            = ap y . ap (fst py)
>      sizeFun         = case mpsize of
>                        Nothing -> const 1
>                        Just _  -> ap size . ap (fst psize)
>      sizes           = map sizeFun rows
>      shapes          = map (shapeFun rows) rows
>      sizedShapes     = zipWith (\x y -> x # scale y) shapes sizes
>      points          = map (\pt -> (xFun pt, yFun pt)) rows

> scaleFromDiscreteScaleInContext :: DiscreteScaleInContext rowT b a -> [rowT] -> DScale b (Colour a)
> scaleFromDiscreteScaleInContext (attr, scale) rows    = scale rows attr

> scaleFromAffineScaleInContext :: AffineScaleInContext rowT -> [rowT] -> AffineScale
> scaleFromAffineScaleInContext   (attr, scale) rows = scale rows attr

--------------------------------------------------------------------------------

> dAttr d attr Nothing           = Just (attr, d)
> dAttr _ attr (Just (_, scale)) = Just (attr, scale)

> withSizeAttr :: A.Attribute rowT Double -> GeomPoint rowT b a -> GeomPoint rowT b a
> withSizeAttr = over geomPointSize . dAttr defaultSizeScaleInContext

> withColorAttr :: (Default d, Ord d, Ord c, Floating c) => A.Attribute rowT d -> GeomPoint rowT d c -> GeomPoint rowT d c
> withColorAttr = over geomPointColor . dAttr defaultColorScaleInContext

> defaultYScaleInContext               = defaultXScaleInContext
> defaultXScaleInContext     rows attr = autoAffineScale   rows attr # slack 1.1
> defaultSizeScaleInContext  rows attr = autoAffineScale   rows attr

> defaultColorScaleInContext :: (Default b, Ord b, Ord a, Floating a) => [rowT] -> A.Attribute rowT b -> DScale b (Colour a)
> defaultColorScaleInContext rows attr = autoDiscreteScale rows attr # rangeXform Plots.ColorBrewer.set1

--------------------------------------------------------------------------------
GeomHLine

> data GeomHLine rowT b a = GeomHLine
>     { _geomHLineY :: Maybe (AffineScaleInContext rowT)
>     }
>
> makeLenses ''GeomHLine

> hline :: Double -> GeomHLine rowT b Double -> [rowT] -> Maybe DC
> hline v geom rows = 
>     do yscale <- yScale geom rows
>        let plotY = ap yscale v
>        return $ (p2 (0.0, plotY)) ~~ (p2 (1.0, plotY)) # lineColor black # lw 0.005 # translate (r2 ((-0.5), (-0.5)))

> geomHLine :: GeomHLine rowT b a
> geomHLine = GeomHLine Nothing

--------------------------------------------------------------------------------
Instances

> instance HasX (GeomPoint rowT b a) where
>     type HasXTarget (GeomPoint rowT b a) = Maybe (AffineScaleInContext rowT)
>     type HasXRowType (GeomPoint rowT b a) = rowT
>     x = geomPointX
>     withXAttr = withAttr x defaultXScaleInContext
>     xScale geom rows = case L.view x geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr

> instance HasY (GeomPoint rowT b a) where
>     type HasYTarget (GeomPoint rowT b a) = Maybe (AffineScaleInContext rowT)
>     type HasYRowType (GeomPoint rowT b a) = rowT
>     y = geomPointY
>     withYAttr = withAttr geomPointY defaultYScaleInContext
>     yScale geom rows = case L.view y geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr

> instance HasY (GeomHLine rowT b a) where
>     type HasYTarget (GeomHLine rowT b a) = Maybe (AffineScaleInContext rowT)
>     type HasYRowType (GeomHLine rowT b a) = rowT
>     y = geomHLineY
>     withYAttr = withAttr geomHLineY defaultYScaleInContext
>     yScale geom rows = case L.view y geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr
