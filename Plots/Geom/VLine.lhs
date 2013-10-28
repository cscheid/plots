> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE TypeFamilies #-}

> module Plots.Geom.VLine (
>  GeomVLine, geomVLine, -- default constructor
>  geomVLineX, -- lenses
>  vline
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
