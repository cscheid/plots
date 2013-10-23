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

> main = mapM_ (print . attributeFun (attrPair sepalLength petalLength)) iris
