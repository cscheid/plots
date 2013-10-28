> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE TypeFamilies #-}

> module Plots.Geom(
>
>  GeomPoint, geomPoint, -- default constructor
>  geomPointX, geomPointY, geomPointSize, geomPointColor, -- lenses
>  splot,
>  withSizeAttr, withColorAttr,
>  defaultXScaleInContext,
>  defaultYScaleInContext,
>  defaultSizeScaleInContext,
>  defaultColorScaleInContext,
>
>  GeomHLine, geomHLine, -- default constructor
>  geomHLineY, -- lenses
>  hline, 
>
>  GeomVLine, geomVLine, -- default constructor
>  geomVLineX, -- lenses
>  vline, 
>
> ) where

> import Data.Default
> import Data.Maybe
> import Diagrams.Coordinates hiding ((&))
> import Diagrams.Prelude hiding (over)
> import Control.Monad hiding (ap)

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

> defaultYScaleInContext               = defaultXScaleInContext
> defaultXScaleInContext     rows attr = autoAffineScale   rows attr # slack 1.1
> defaultSizeScaleInContext  rows attr = autoAffineScale   rows attr

> defaultColorScaleInContext :: (Default b, Ord b, Ord a, Floating a) => [rowT] -> A.Attribute rowT b -> DScale b (Colour a)
> defaultColorScaleInContext rows attr = autoDiscreteScale rows attr # rangeXform Plots.ColorBrewer.set1

--------------------------------------------------------------------------------
GeomPoint

> data GeomPoint rowT b a = GeomPoint
>     { _geomPointX     :: Maybe (AffineScaleInContext rowT),
>       _geomPointY     :: Maybe (AffineScaleInContext rowT),
>       _geomPointSize  :: Maybe (AffineScaleInContext rowT),
>       _geomPointColor :: Maybe (DiscreteScaleInContext rowT b a),
>       _geomPointData  :: Maybe [rowT]
>     }
>
> makeLenses ''GeomPoint

> geomPoint :: GeomPoint rowT b a
> geomPoint = GeomPoint Nothing Nothing Nothing Nothing Nothing

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

> instance HasData (GeomPoint rowT b a) where
>     type HasDataTarget  (GeomPoint rowT b a) = Maybe [rowT]
>     type HasDataRowType (GeomPoint rowT b a) = rowT
>     data_ = geomPointData

--------------------------------------------------------------------------------

FIXME make this into Maybe DC when mpx or mpy are Nothing

> splot :: GeomPoint rowT b Double -> [rowT] -> DC
> splot (GeomPoint mpx mpy mpsize mpcolor data_) rows = scatterplot 
>     where
>      rowsToDraw      = fromJust $ msum [data_, Just rows]
>      shapeFun rows x = circle 0.01 # fc (color x) # lc white # lw 0.002
>          where color = case mpcolor of 
>                        Nothing -> const black
>                        Just _ -> ap (scaleFromDiscreteScaleInContext pcolor rows) . ap (fst pcolor)
>      scatterplot     = (mconcat $ zipWith translated sizedShapes points) # translate (r2 (-0.5, (-0.5)))
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
>      sizes           = map sizeFun rowsToDraw
>      shapes          = map (shapeFun rows) rowsToDraw
>      sizedShapes     = zipWith (\x y -> x # scale y) shapes sizes
>      points          = map (\pt -> (xFun pt, yFun pt)) rows

--------------------------------------------------------------------------------
GeomHLine

> data GeomHLine rowT b a = GeomHLine
>     { _geomHLineY :: Maybe (AffineScaleInContext rowT),
>       _geomHLineCoordinate :: Double
>     }
>
> makeLenses ''GeomHLine

> hline :: GeomHLine rowT b Double -> [rowT] -> Maybe DC
> hline geom rows = 
>     do yscale <- yScale geom rows
>        let plotY = ap yscale (L.view geomHLineCoordinate geom)
>        return $ (p2 (0.0, plotY)) ~~ (p2 (1.0, plotY)) # lineColor black # lw 0.005 # translate (r2 ((-0.5), (-0.5)))

> geomHLine :: Double -> GeomHLine rowT b a
> geomHLine coord = GeomHLine Nothing coord

> instance HasY (GeomHLine rowT b a) where
>     type HasYTarget (GeomHLine rowT b a) = Maybe (AffineScaleInContext rowT)
>     type HasYRowType (GeomHLine rowT b a) = rowT
>     y = geomHLineY
>     withYAttr = withAttr geomHLineY defaultYScaleInContext
>     yScale geom rows = case L.view y geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr

--------------------------------------------------------------------------------
GeomVLine

> data GeomVLine rowT b a = GeomVLine
>     { _geomVLineX :: Maybe (AffineScaleInContext rowT),
>       _geomVLineCoordinate :: Double
>     }
>
> makeLenses ''GeomVLine

> vline :: GeomVLine rowT b Double -> [rowT] -> Maybe DC
> vline geom rows = 
>     do xscale <- xScale geom rows
>        let plotX = ap xscale (L.view geomVLineCoordinate geom)
>        return $ (p2 (plotX, 0.0)) ~~ (p2 (plotX, 1.0)) # lineColor black # lw 0.005 # translate (r2 ((-0.5), (-0.5)))

> geomVLine :: Double -> GeomVLine rowT b a
> geomVLine coord = GeomVLine Nothing coord

> instance HasX (GeomVLine rowT b a) where
>     type HasXTarget (GeomVLine rowT b a) = Maybe (AffineScaleInContext rowT)
>     type HasXRowType (GeomVLine rowT b a) = rowT
>     x = geomVLineX
>     withXAttr = withAttr geomVLineX defaultXScaleInContext
>     xScale geom rows = case L.view x geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr

--------------------------------------------------------------------------------

> dAttr d attr Nothing           = Just (attr, d)
> dAttr _ attr (Just (_, scale)) = Just (attr, scale)

> withSizeAttr :: A.Attribute rowT Double -> GeomPoint rowT b a -> GeomPoint rowT b a
> withSizeAttr = over geomPointSize . dAttr defaultSizeScaleInContext

> withColorAttr :: (Default d, Ord d, Ord c, Floating c) => A.Attribute rowT d -> GeomPoint rowT d c -> GeomPoint rowT d c
> withColorAttr = over geomPointColor . dAttr defaultColorScaleInContext
