> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE TypeFamilies #-}

> module Plots.Geom.HLine (
>  GeomHLine, geomHLine, -- default constructor
>  geomHLineY, -- lenses
>  hline, 
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
