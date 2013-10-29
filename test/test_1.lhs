> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Backend.SVG.CmdLine
> import Diagrams.Backend.SVG
> import Diagrams.TwoD.Size

> import Plots
> import Plots.Datasets

--------------------------------------------------------------------------------
the scales

> type IrisRow = (Double, Double, Double, Double, String)

> geomPoint1 :: GeomPoint IrisRow String Double
> geomPoint1 = geomPoint # withXAttr sepalLength # withYAttr petalLength

> geomPoint2 = geomPoint1 # withColorAttr species
> geomPoint3 = geomPoint1 # withSizeAttr sepalWidth
> geomPoint4 = geomPoint1 # withSizeAttr sepalWidth # withColorAttr species
> geomPoint5 = geomPoint1 # withXAttr petalWidth

> geomHLine1 :: GeomHLine IrisRow String Double
> geomHLine1 = geomHLine 5 # withYAttr petalLength

> testPlot = plot # withData iris
>                 # withXAttr sepalLength
>                 # withYAttr petalLength
>                 # addLayer (geomHLine 5)
>                 # addLayer (geomHLine 3)
>                 # addLayer (geomVLine 6)
>                 # addLayer (geomPoint # withColorAttr species)
>                 # addLayer (geomABLine 1 0)

> main = do renderSVG "out.svg" (Height 600) (draw testPlot)
>           renderSVG "out1.svg" (Height 600) (draw $ testPlot # withXAttr petalWidth)

