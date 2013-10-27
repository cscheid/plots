> {-# LANGUAGE TemplateHaskell #-}
> module Plots.Plot where

> import Plots.Geom
> import Plots.Scales
> import qualified Plots.Attributes as A
> import Control.Lens hiding ((#))
> import Control.Lens.TH

> data Plot rowT = Plot 
>     { _plotData :: Maybe [rowT],
>       _plotX    :: Maybe (AffineScaleInContext rowT),
>       _plotY    :: Maybe (AffineScaleInContext rowT)
>     }
>
> makeLenses ''Plot

> plot :: Plot rowT
> plot = Plot Nothing Nothing Nothing

