> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE Rank2Types #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE InstanceSigs #-}
> module Plots.Plot where

> import Diagrams.Prelude hiding (view, over)
> import Plots.DiagramUtils
> import Plots.Decorations
> import Plots.Geom
> import Plots.Lens
> import Plots.Scales
> import qualified Diagrams.Prelude as D
> import qualified Plots.Attributes as A
> import Control.Lens hiding ((#))
> import Control.Lens.TH
> import Control.Monad
> import Data.Maybe
> import Data.List

> data Layer rowT b a = LayerPoint  (GeomPoint rowT b a)
>                     | LayerHLine  (GeomHLine rowT b a)
>                     | LayerVLine  (GeomVLine rowT b a)
> --                    | LayerABLine (GeomABLine rowT b a)

> data Plot rowT b a = Plot 
>     { _plotData   :: Maybe [rowT],
>       _plotX      :: Maybe (AffineScaleInContext rowT),
>       _plotY      :: Maybe (AffineScaleInContext rowT),
>       _plotLayers :: [Layer rowT b a]
>     }
>
> makeLenses ''Plot

> plot :: Plot rowT b a
> plot = Plot Nothing Nothing Nothing []

> instance HasData (Plot rowT b a) where
>     type HasDataTarget  (Plot rowT b a) = Maybe [rowT]
>     type HasDataRowType (Plot rowT b a) = rowT
>     data_ = plotData

--------------------------------------------------------------------------------

> instance HasX (Plot rowT b a) where
>     type HasXTarget (Plot rowT b a) = Maybe (AffineScaleInContext rowT)
>     type HasXRowType (Plot rowT b a) = rowT
>     x = plotX
>     withXAttr = withAttr x defaultXScaleInContext
>     xScale geom rows = case view x geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr

> instance HasY (Plot rowT b a) where
>     type HasYTarget (Plot rowT b a) = Maybe (AffineScaleInContext rowT)
>     type HasYRowType (Plot rowT b a) = rowT
>     y = plotY
>     withYAttr = withAttr plotY defaultYScaleInContext
>     yScale geom rows = case view y geom of
>                       Nothing        -> Nothing
>                       Just (attr, s) -> Just $ s rows attr

> class IsLayer f where
>     type LayerRowType f
>     type LayerStringType f
>     type LayerColourType f
>     toLayer :: f -> Layer (LayerRowType f) (LayerStringType f) (LayerColourType f)

> instance IsLayer (GeomPoint rowT b a) where
>     type LayerRowType    (GeomPoint rowT b a) = rowT
>     type LayerStringType (GeomPoint rowT b a) = b
>     type LayerColourType (GeomPoint rowT b a) = a
>     toLayer p = LayerPoint p

> instance IsLayer (GeomHLine rowT b a) where
>     type LayerRowType    (GeomHLine rowT b a) = rowT
>     type LayerStringType (GeomHLine rowT b a) = b
>     type LayerColourType (GeomHLine rowT b a) = a
>     toLayer p = LayerHLine p

> instance IsLayer (GeomVLine rowT b a) where
>     type LayerRowType    (GeomVLine rowT b a) = rowT
>     type LayerStringType (GeomVLine rowT b a) = b
>     type LayerColourType (GeomVLine rowT b a) = a
>     toLayer p = LayerVLine p

-- > instance IsLayer (GeomABLine rowT b a) where
-- >     type LayerRowType    (GeomABLine rowT b a) = rowT
-- >     type LayerStringType (GeomABLine rowT b a) = b
-- >     type LayerColourType (GeomABLine rowT b a) = a
-- >     toLayer p = LayerABLine p

> addLayer :: IsLayer f => 
>      f
>      -> Plot (LayerRowType f) (LayerStringType f) (LayerColourType f)
>      -> Plot (LayerRowType f) (LayerStringType f) (LayerColourType f)
> addLayer l = over plotLayers (toLayer l :)

> layerX (LayerPoint a)  = view x a
> layerX (LayerHLine _)  = Nothing
> layerX (LayerVLine a)  = view x a
> -- layerX (LayerABLine a) = view x a

> layerY (LayerPoint a)  = view y a
> layerY (LayerHLine a)  = view y a
> layerY (LayerVLine _)  = Nothing
> -- layerY (LayerABLine a) = view y a

> plotXScale :: Plot rowT b a -> Maybe AffineScale
> plotXScale plot = fmap (flip scaleFromAffineScaleInContext theData) s
>     where
>     s        = msum $ (view x plot : map layerX (view plotLayers plot))
>     theData  = fromJust (view plotData plot)

> plotYScale :: Plot rowT b a -> Maybe AffineScale
> plotYScale plot = fmap (flip scaleFromAffineScaleInContext theData) s
>     where
>     s        = msum $ (view y plot : map layerY (view plotLayers plot))
>     theData  = fromJust (view plotData plot)

> setLayerScales :: Plot rowT b a -> Layer rowT b a -> Layer rowT b a
> setLayerScales plot (LayerPoint geomPoint) =
>     LayerPoint (geomPoint # set x sx # set y sy)
>     where
>     sx = msum [view x geomPoint, view x plot]
>     sy = msum [view y geomPoint, view y plot]
> setLayerScales plot (LayerHLine geomHLine) =
>     LayerHLine (set y sy geomHLine)
>     where
>     sy = msum [view y geomHLine, view y plot]
> setLayerScales plot (LayerVLine geomVLine) =
>     LayerVLine (set x sx geomVLine)
>     where
>     sx = msum [view x geomVLine, view x plot]

-- > setLayerScales plot (LayerABLine geomABLine) =
-- >     LayerPoint (geomABLine # set x sx # set y sy)
-- >     where
-- >     sx = msum [view x geomABLine, view x plot]
-- >     sy = msum [view y geomABLine, view y plot]

> draw :: Show b => Plot rowT b Double -> DC
> draw plot = (addLegends (layersDiagram <> backgroundGrid xScale yScale) legends) # pad 1.2
>     where
>     xScale          = fromJust (plotXScale plot)
>     yScale          = fromJust (plotYScale plot)
>     theData         = fromJust (view plotData plot)
>     layersDiagram   = mconcat layers
>     layers          = map (D.view (p2 (-0.5,-0.5)) (r2 (1,1)) . drawLayer theData . setLayerScales plot) (view plotLayers plot)
>     legends         = concatMap (layerLegends theData) (view plotLayers plot)
>     addLegends plot []   = plot
>     addLegends plot legs = plot ||| strutX 0.1 ||| (foldr1 (===) (intersperse (strutY 0.05) legs))

> drawLayer :: [rowT] -> Layer rowT b Double -> DC
> drawLayer rows (LayerPoint point) = splot point rows
> drawLayer rows (LayerHLine line)  = safeFromJust (hline  line rows)
> drawLayer rows (LayerVLine line)  = safeFromJust (vline  line rows)
> -- drawLayer rows (LayerABLine line) = safeFromJust (abline line rows)

> layerLegends :: Show b => [rowT] -> Layer rowT b Double -> [DC]
> layerLegends rows (LayerPoint point) = pointLegends point rows
> layerLegends rows (LayerHLine line)  = []
> layerLegends rows (LayerVLine line)  = []
> -- layerLegends rows (LayerABLine line) = []
