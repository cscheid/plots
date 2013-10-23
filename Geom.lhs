> module Geom where

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Backend.SVG
> import Diagrams.Coordinates
> import Diagrams.Prelude hiding (apply)
> import Graphics.SVGFonts.ReadFont
> import Diagrams.Backend.SVG.CmdLine
> import Data.Colour.SRGB.Linear
> import Data.Colour.SRGB
> import Diagrams.TwoD.Text
> import Numeric
> import Data.List
> import Data.Default

> import Iso
> import DiagramUtils
> import Scales
> import Datasets
> import Attributes
> import qualified ColorBrewer

> data GeomPoint rowT b a = GeomPoint 
>     { pointGeomXAttr      :: Attributes.Attribute rowT Double,
>       pointGeomXScale     :: [rowT] -> Attributes.Attribute rowT Double -> AffineScale,
>
>       pointGeomYAttr      :: Attributes.Attribute rowT Double,
>       pointGeomYScale     :: [rowT] -> Attributes.Attribute rowT Double -> AffineScale,
>
>       pointGeomSizeAttr   :: Attributes.Attribute rowT Double,
>       pointGeomSizeScale  :: [rowT] -> Attributes.Attribute rowT Double -> AffineScale,
>
>       pointGeomColorAttr  :: Attributes.Attribute rowT b,
>       pointGeomColorScale :: [rowT] -> Attributes.Attribute rowT b -> DScale b (Colour a) }

> splot :: GeomPoint rowT b Double -> [rowT] -> DC
> splot (GeomPoint xattr xscale yattr yscale sattr sscale cattr cscale) rows =
>     scatterplot where
>      shapeFun rows x = circle 0.01 # fc color # lc white # lw 0.002
>          where color = ap (cscale rows cattr) $ attributeFun cattr x
>      scatterplot     = (view (p2 (0,0)) (r2 (1,1)) $ mconcat $ zipWith translated sizedShapes points) # translate ((-0.5) & (-0.5))
>      x               = xscale rows xattr
>      y               = yscale rows yattr
>      size            = sscale rows sattr
>      xFun            = ap x    . attributeFun xattr
>      yFun            = ap y    . attributeFun yattr
>      sizeFun         = ap size . attributeFun sattr
>      sizes           = map sizeFun rows
>      shapes          = map (shapeFun rows) rows
>      sizedShapes     = zipWith (\x y -> x # scale y) shapes sizes
>      points          = map (\pt -> (xFun pt, yFun pt)) rows
