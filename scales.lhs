= Scales

> module Scales where

> import Diagrams.Backend.SVG
> import Diagrams.Coordinates
> import Diagrams.Prelude hiding (apply)
> import Graphics.SVGFonts.ReadFont
> import Diagrams.Backend.SVG.CmdLine
> import Data.Colour.SRGB.Linear
> import Diagrams.TwoD.Text
> import Numeric
> import Data.List
> import Data.Default
> import qualified Attributes

> import Iso
> import DiagramUtils
> import qualified Data.Set

== Interval Scales

Interval scales are scales of the form r . d, where r and d are range
and domain isomorphisms; r are functions from [0, 1] -> b, and d are
functions from a -> [0, 1].

This includes the typical "linear scales" of scatterplot and histogram
axes, many "continuous color scales" of interest, (including
"non-linear" color scales in device space, by defining appropriate
curvilinear coordinates like HLS)

Interval scales try to capture the property that these scales are,
fundamentally, injective mappings from a closed interval to a set.

In addition, interval scales keep track of their domain and range
interval endpoints, and have names (for displaying purposes only)

> data IntervalScale a b = IntervalScale 
>     { intervalScaleRangeXform :: Iso Double b,
>       intervalScaleDomainXform :: Iso a Double, 
>       intervalScaleRange :: (b, b),
>       intervalScaleDomain :: (a, a),
>       intervalScaleName :: String
>     }

> fundamentalIntervalScale :: IntervalScale Double Double
> fundamentalIntervalScale = IntervalScale 
>     { intervalScaleRangeXform = Iso id id,
>       intervalScaleDomainXform = Iso id id,
>       intervalScaleRange = (0.0, 1.0),
>       intervalScaleDomain = (0.0, 1.0),
>       intervalScaleName = "<unnamed>"
>     }

> intervalScaleDomainTransformation :: Isomorphism f => f a b -> IntervalScale b c -> IntervalScale a c
> intervalScaleDomainTransformation iso_new s = s
>     { intervalScaleDomainXform = d_old `o` d_new,
>       intervalScaleDomain = (ap (Iso.inv d_new) old_min, 
>                              ap (Iso.inv d_new) old_max)
>     } 
>     where
>     d_new = toIso iso_new
>     d_old = intervalScaleDomainXform s
>     (old_min, old_max) = intervalScaleDomain s

> intervalScaleRangeTransformation :: Isomorphism f => f b c -> IntervalScale a b -> IntervalScale a c
> intervalScaleRangeTransformation iso_new s = s
>     { intervalScaleRangeXform = d_new `o` d_old,
>       intervalScaleRange = (ap d_new old_min, 
>                             ap d_new old_max)
>     } 
>     where
>     d_new = toIso iso_new
>     d_old = intervalScaleRangeXform s
>     (old_min, old_max) = intervalScaleRange s

> intervalScaleRename :: String -> IntervalScale a b -> IntervalScale a b
> intervalScaleRename x scale = scale { intervalScaleName = x }

=== Interval Scales are Isomorphisms

> intervalScaleApply :: IntervalScale a b -> a -> b
> intervalScaleApply scale a = apply (g `o` f) a
>     where g = intervalScaleRangeXform scale
>           f = intervalScaleDomainXform scale

> intervalScaleInverse :: IntervalScale a b -> IntervalScale b a
> intervalScaleInverse (IntervalScale r d br bd n) = 
>     IntervalScale (Iso.inv d) (Iso.inv r) bd br n

There are two ways of composing intervalScales. They're not exactly
symmetric because either the range transformation of g.f will include
pieces of f or the domain transformation of g.f will contain pieces
of g. I think either one satisfies the isomorphism laws, though, so it
should be ok.

> intervalScaleCompose :: IntervalScale b c -> IntervalScale a b -> IntervalScale a c
> intervalScaleCompose g f = IntervalScale r d br_g bd_f n where
>     r = r_g `o` d_g `o` r_f
>     d = d_f
>     IntervalScale r_g d_g br_g bd_g n_g = g
>     IntervalScale r_f d_f br_f bd_f n_f = f
>     n = n_g ++ " . " ++ n_f

> intervalScaleCompose' :: IntervalScale b c -> IntervalScale a b -> IntervalScale a c
> intervalScaleCompose' g f = IntervalScale r d br_g bd_f n where
>     r = r_g
>     d = d_g `o` r_f `o` d_f
>     IntervalScale r_g d_g br_g bd_g n_g = g
>     IntervalScale r_f d_f br_f bd_f n_f = f
>     n = n_g ++ " . " ++ n_f

As it says on the tin.

> instance Isomorphism IntervalScale where
>     ap = intervalScaleApply
>     inv = intervalScaleInverse
>     o = intervalScaleCompose

--------------------------------------------------------------------------------

=== Affine scales

Affine scales are the equivalent of d3's "linear" scales. Since they include a
translation term, those are also really affine.

> affineScale :: (Double, Double) -> (Double, Double) -> IntervalScale Double Double
> affineScale (from1, from2) (to1, to2) =
>     intervalScaleDomainTransformation domainIso .
>     intervalScaleRangeTransformation rangeIso $ fundamentalIntervalScale
>     where domainIso = Iso from12ToZero1 zero1ToFrom12
>           rangeIso = Iso zero1ToTo12 to12ToZero1
>           xyToZero1 x y v = (v - x) / (y - x)
>           zero1ToXY x y v = v * (y - x) + x
>           from12ToZero1 = xyToZero1 from1 from2
>           zero1ToFrom12 = zero1ToXY from1 from2
>           zero1ToTo12 = zero1ToXY to1 to2
>           to12ToZero1 = xyToZero1 to1 to2

> autoAffineScale :: [a] -> Attributes.Attribute a Double -> IntervalScale Double Double
> autoAffineScale rows (Attributes.MkAttribute (selector, name))
>     = affineScale (mn, mx) (0, 1) # intervalScaleRename name
>       where vs = map selector rows
>             mn = foldr1 min vs
>             mx = foldr1 max vs

> slack :: Double -> IntervalScale Double Double -> IntervalScale Double Double
> slack amount scale = intervalScaleDomainTransformation domainIso scale
>     where (old_min, old_max) = intervalScaleDomain scale
>           half_span = (old_max - old_min) / 2
>           center = (old_max + old_min) / 2
>           new_domain@(new_min, new_max) = (center - half_span * amount, center + half_span * amount)
>           domainIso = Iso ab ba
>           ab = intervalScaleApply ls
>           ba = intervalScaleApply (intervalScaleInverse ls)
>           ls = affineScale (new_min, new_max) (old_min, old_max)

> sizeScaleLegend :: DC -> IntervalScale Double Double -> DC
> sizeScaleLegend shape sizeScale = ((strutY 1.5 === alignedText 0 0 title) # scale 0.04) # alignL === (bounded_shapes ||| strutX 0.05 ||| tickMarks) # alignL
>     where title = intervalScaleName sizeScale
>           sTicks = ticks (intervalScaleDomain sizeScale) 5
>           sizes = map (ap sizeScale) sTicks
>           shapes = map (\s -> shape # lineColor transparent # fc black # scale s) $ sizes
>           all_phantoms = phantom $ mconcat shapes
>           bounded_shapes = foldr1 (===) . intersperse (strutY 0.01) . map (\s -> all_phantoms <> s) $ shapes
>           tickMarks = foldr1 (===) . intersperse (strutY 0.01) . map (\s -> all_phantoms <> (text (show s) # scale 0.04)) $ sTicks

--------------------------------------------------------------------------------

== Discrete Scales

Discrete scales represent injective maps between finite sets, encoded
as lists. This is an inefficient encoding, but is extremely
convenient. Since I expect the size of these maps to be small, they'll be
`lookup`s until there's evidence that something better is needed.

=== Shortcomings

The DScale type fails to encode the size of the lists. This would be
necessary to prove the isomorphism laws precisely, since then
composing DScales of different cardinalities wouldn't work. So *don't
compose DScales of different cardinalities*.

In addition, DScale is only an isomorphism when restricted to the
values in dScaleRange and dScaleDomain. dScaleRangeDefault and
dScaleDomainDefault are there to make the isomorphism a total
function, so I can keep the code free of 'error's. This is probably
somehow better encoded with Maybe.

> data DScale a b = DScale { dScaleRange :: [b],
>                            dScaleRangeDefault :: b,
>                            dScaleDomain :: [a],
>                            dScaleDomainDefault :: a,
>                            dScaleIso :: Iso a b,
>                            dScaleName :: String }

FIXME: What do I do about the name in inv?

> instance Isomorphism DScale where
>     ap scale = ap (dScaleIso scale)
>     inv (DScale vals dVal keys dKey iso name) = DScale keys dKey vals dVal (Iso.inv iso) name
>     scale_2 `o` scale_1 = DScale vals_2 dVal_2 keys_1 dKey_1 (iso_2 `o` iso_1) (name_2 ++ " . " ++ name_1)
>         where
>         DScale _      _      keys_1 dKey_1 iso_1 name_1 = scale_1
>         DScale vals_2 dVal_2 _      _      iso_2 name_2 = scale_2

> functionFromListPairs :: Eq a => [a] -> [b] -> b -> a -> b
> functionFromListPairs keys values ifNotFound key =
>     case lookup key (zip keys values) of
>     Nothing -> ifNotFound
>     Just v  -> v

> discreteScale :: (Eq a, Eq b) => [a] -> a -> [b] -> b -> String -> DScale a b
> discreteScale keys dKey vals dVal name =
>     DScale vals dVal keys dKey (Iso ab ba) name
>     where ab = functionFromListPairs keys vals dVal
>           ba = functionFromListPairs vals keys dKey

> discreteScale' :: Isomorphism f => [a] -> a -> b -> (f a b) -> String -> DScale a b
> discreteScale' keys dKey dVal valIso name =
>     DScale { dScaleDomain = keys,
>              dScaleDomainDefault = dKey,
>              dScaleRange = map (ap valIso) keys,
>              dScaleRangeDefault = dVal,
>              dScaleIso = toIso valIso,
>              dScaleName = name
>              }

> autoDiscreteScale :: (Ord b, Eq c) => [a] -> Attributes.Attribute a b -> b -> ([c], c) -> DScale b c
> autoDiscreteScale dataSet attr@(Attributes.MkAttribute (_, name)) defKey (scaleValues, defVal) = discreteScale' scaleKeys defKey defVal scaleIso name
>     where scaleKeys = image dataSet attr
>           scaleAB = functionFromListPairs scaleKeys scaleValues defVal
>           scaleBA = functionFromListPairs scaleValues scaleKeys defKey
>           scaleIso = Iso scaleAB scaleBA
>           image :: Ord b => [a] -> Attributes.Attribute a b -> [b]
>           image dataSet (Attributes.MkAttribute (fn, _)) = Data.Set.toList imageSet
>                 where imageList = map fn dataSet
>                       imageSet = Data.Set.fromList imageList

--------------------------------------------------------------------------------
ticks, legends, bah

> ticks :: (Double, Double) -> Double -> [Double]
> ticks d m = takeWhile (< b) [a, a+c ..] where (a, b, c) = tickRange d m

Choose ticks sensibly, algorithm stolen from d3

> tickRange :: (Double, Double) -> Double -> (Double, Double, Double)
> tickRange (mn, mx) m = 
>     (fromIntegral (ceiling (mn / step)) * step,
>      fromIntegral (floor (mx / step)) * step + (step / 2.0),
>      step)
>     where span = mx - mn
>           step' = 10.0 ** fromIntegral (floor (logBase 10 (span / m)))
>           err = (m / span) * step'
>           step = if err <= 0.15 then step' * 10 else
>                  if err <= 0.35 then step' * 5 else
>                  if err <= 0.75 then step' * 2 else step'

> colorLegend :: Show b => DScale b (Colour Double) -> DC
> colorLegend cscale = (strutY 1.5 === alignedText 0 0 title # alignL === (foldr1 (===) $ intersperse (strutY 0.2) (zipWith colorEntry vs cs)) # alignL) # scale 0.04 
>     where title = dScaleName cscale
>           vs = dScaleDomain cscale
>           cs = dScaleRange cscale
>           colorEntry name color = square 1 # fc color # lineColor transparent ||| (alignedText 0 0.5 (show name)) # translate (r2 (1, 0))

--------------------------------------------------------------------------------

backgroundGrid draws a background grid for your typical scatterplot drawing.

> backgroundGrid :: IntervalScale Double Double -> IntervalScale Double Double -> DC
> backgroundGrid xScale yScale = v |||> ((hLines <> vLines <> bg) === h)
>     where xTitle = intervalScaleName xScale
>           yTitle = intervalScaleName yScale
>           bg = rect 1 1 # translate (r2 (0.5, 0.5))
>                         # fc (rgb 0.9 0.9 0.9)
>                         # lineColor transparent
>                         # centerXY
>           niceShow x = showFFloat (Just 2) x "" -- FIXME
>           alphaFromTick t 
>               | fromIntegral (floor t) == t = 1
>               | otherwise = (alphaFromTick (t * 10)) / 2
>           vTicks = ticks (intervalScaleDomain xScale) 10
>           hTicks = ticks (intervalScaleDomain yScale) 10
>           vTickLocations = map (\d -> intervalScaleApply xScale d) vTicks
>           hTickLocations = map (\d -> intervalScaleApply yScale d) hTicks
>           vLines = (mconcat $ zipWith (\x t -> (x & 0.0) ~~ (x & 1.0) # lineColor (white `withOpacity` alphaFromTick t) # lw 0.005) vTickLocations vTicks) # translate ((-0.5) & (-0.5))
>           hLines = (mconcat $ zipWith (\y t -> (0.0 & y) ~~ (1.0 & y) # lineColor (white `withOpacity` alphaFromTick t) # lw 0.005) hTickLocations hTicks) # translate ((-0.5) & (-0.5))
>           vTickMarks = (mconcat $ zipWith (\location v -> text (niceShow v) # scale 0.04 # translate (r2 (location, 0))) vTickLocations vTicks) # withEnvelope (rect 1 0.06 # translate (0.5 & 0.0) :: D R2)
>           hTickMarks = (mconcat $ zipWith (\location v -> alignedText 0 0.5 (niceShow v) # scale 0.04 # translate (r2 ((-0.05), location))) hTickLocations hTicks) # withEnvelope (rect 0.1 1 # translate (0.0 & 0.5) :: D R2)
>           xScaleTitle = text xTitle # scale 0.04 # centerX # withEnvelope (rect 1 0.04 :: D R2)
>           yScaleTitle = text yTitle # scale 0.04 # rotateBy (1/4) # withEnvelope (rect 0.06 1 :: D R2)
>           v = (yScaleTitle # centerY) ||| (hTickMarks # centerY)
>           h = (vTickMarks # centerX) === (xScaleTitle # centerX)


--------------------------------------------------------------------------------

As Jacob points out,

"so i think you can make an Injective a b class pretty easy. it has two methods

 fwd :: a -> b
 rev :: b -> Maybe a

 and two laws:

 rev . fwd = Just
 fmap fw . rev = id"

With this, scales should be Injective and Isomorphic.
