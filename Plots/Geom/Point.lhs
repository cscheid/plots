> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE TypeFamilies #-}

> module Plots.Geom.Point (
>  GeomPoint, geomPoint, -- default constructor
>  geomPointX, geomPointY, geomPointSize, geomPointColor, -- lenses
>  splot,
>  withSizeAttr, withColorAttr
> ) where

> import qualified Control.Lens as L
> import Control.Lens.TH
> import Control.Monad hiding (ap)
> import Data.Default
> import Data.Maybe
> import Diagrams.Prelude

> import qualified Plots.Attributes as A
> import Plots.DiagramUtils
> import Plots.Geom.Prelude
> import Plots.Lens
> import Plots.Iso
> import Plots.Scales

--------------------------------------------------------------------------------

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

> withSizeAttr :: A.Attribute rowT Double -> GeomPoint rowT b a -> GeomPoint rowT b a
> withSizeAttr = L.over geomPointSize . dAttr defaultSizeScaleInContext

> withColorAttr :: (Default d, Ord d, Ord c, Floating c) => A.Attribute rowT d -> GeomPoint rowT d c -> GeomPoint rowT d c
> withColorAttr = L.over geomPointColor . dAttr defaultColorScaleInContext
