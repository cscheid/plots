> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE TypeFamilies #-}

> module Plots.Geom.ABLine (
>  GeomABLine, geomABLine, -- default constructor
>  geomABLineY, geomABLineX, geomABLineSlope, geomABLineIntercept, geomABLineMinimum, geomABLineMaximum, -- lenses
>  abline
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

--------------------------------------------------------------------------------

> data GeomABLine rowT b a = GeomABLine
>     { _geomABLineX         :: Maybe (AffineScaleInContext rowT),
>       _geomABLineY         :: Maybe (AffineScaleInContext rowT),
>       _geomABLineMinimum   :: Maybe Double,
>       _geomABLineMaximum   :: Maybe Double,
>       _geomABLineSlope     :: Double,
>       _geomABLineIntercept :: Double
>     }
>
> makeLenses ''GeomABLine

> geomABLine :: Double -> Double -> GeomABLine rowT b a
> geomABLine = GeomABLine Nothing Nothing Nothing Nothing

> instance HasX (GeomABLine rowT b a) where
>     type HasXTarget (GeomABLine rowT b a) = Maybe (AffineScaleInContext rowT)
>     type HasXRowType (GeomABLine rowT b a) = rowT
>     x = geomABLineX
>     withXAttr = withAttr geomABLineX defaultXScaleInContext
>     xScale geom rows = case L.view x geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr

> instance HasY (GeomABLine rowT b a) where
>     type HasYTarget (GeomABLine rowT b a) = Maybe (AffineScaleInContext rowT)
>     type HasYRowType (GeomABLine rowT b a) = rowT
>     y = geomABLineY
>     withYAttr = withAttr geomABLineY defaultYScaleInContext
>     yScale geom rows = case L.view y geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr

> abline :: GeomABLine rowT b Double -> [rowT] -> Maybe DC
> abline geom rows =
>     do let dataSlope     = L.view geomABLineSlope geom
>        let dataIntersect = L.view geomABLineIntercept geom
>        let v x = dataSlope * x + dataIntersect
>        xscale <- xScale geom rows
>        yscale <- yScale geom rows
>        let dataAp x = Just $ ap (inv xscale) x
>        let dataLeft   = fromJust $ msum [L.view geomABLineMinimum geom, dataAp 0]
>        let dataRight  = fromJust $ msum [L.view geomABLineMaximum geom, dataAp 1]
>        let dataLeftY  = v dataLeft
>        let dataRightY = v dataRight
>        let l = p2 (ap xscale dataLeft,  ap yscale dataLeftY)
>        let r = p2 (ap xscale dataRight, ap yscale dataRightY)
>        return $ l ~~ r # lineColor black
>                        # lw 0.005
>                        # translate (r2 ((-0.5), (-0.5)))
