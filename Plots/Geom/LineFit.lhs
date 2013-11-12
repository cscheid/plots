> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE TypeFamilies #-}

> module Plots.Geom.LineFit (
>  GeomLineFit, geomLineFit,
>  lineFit -- default constructor
> ) where

> import qualified Control.Lens as L
> import Control.Lens.TH
> import Control.Monad hiding (ap)
> import Data.Default
> import Data.Maybe
> import Diagrams.Prelude hiding (inv)

> import qualified Plots.Attributes as A
> import Plots.DiagramUtils
> import Plots.Geom.Prelude
> import Plots.Lens
> import Plots.Iso
> import Plots.Scales

> import Plots.Geom.ABLine
> import Plots.Regression.LeastSquares
> import System.IO.Unsafe

--------------------------------------------------------------------------------

> data GeomLineFit rowT b a = GeomLineFit
>     { _geomLineFitX :: Maybe (AffineScaleInContext rowT),
>       _geomLineFitY :: Maybe (AffineScaleInContext rowT)
>     }
>
> makeLenses ''GeomLineFit

> geomLineFit = GeomLineFit Nothing Nothing

> instance HasX (GeomLineFit rowT b a) where
>     type HasXTarget (GeomLineFit rowT b a) = Maybe (AffineScaleInContext rowT)
>     type HasXRowType (GeomLineFit rowT b a) = rowT
>     x = geomLineFitX
>     withXAttr = withAttr geomLineFitX defaultXScaleInContext
>     xScale geom rows = case L.view x geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr

> instance HasY (GeomLineFit rowT b a) where
>     type HasYTarget (GeomLineFit rowT b a) = Maybe (AffineScaleInContext rowT)
>     type HasYRowType (GeomLineFit rowT b a) = rowT
>     y = geomLineFitY
>     withYAttr = withAttr geomLineFitY defaultYScaleInContext
>     yScale geom rows = case L.view y geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr

> lineFit :: GeomLineFit rowT b Double -> [rowT] -> Maybe DC
> lineFit geom rows =
>     do geomX <- L.view geomLineFitX geom
>        geomY <- L.view geomLineFitY geom
>        let xs = map (ap (fst geomX)) rows
>        let ys = map (ap (fst geomY)) rows
>        let fit = linearLeastSquares $ zip xs ys
>        let intercept = fit 0.0
>        let slope = fit 1.0 - intercept
>        abline (geomABLine slope intercept # L.set x (L.view x geom)
>                                           # L.set y (L.view y geom)
>                                           # L.set geomABLineMinimum (Just $ minimum xs)
>                                           # L.set geomABLineMaximum (Just $ maximum xs)
>                ) rows
