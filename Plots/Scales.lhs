= Scales

> module Plots.Scales where

> import Diagrams.Coordinates
> import Diagrams.Prelude
> import Data.Colour.SRGB.Linear
> import Numeric

> import Data.Default
> import qualified Plots.Iso as Iso
> import Plots.DiagramUtils
> import qualified Data.Set
> import qualified Plots.Attributes as A

== Scales typeclass

This is currently unused for now.

> class Scale s where
>     name        :: s a b -> String
>     rename      :: String -> s a b -> s a b
>     rangeXform  :: Iso.Isomorphism f => f b c -> s a b -> s a c
>     domainXform :: Iso.Isomorphism f => f a b -> s b c -> s a c

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
>     { intervalScaleRangeXform :: Iso.Iso Double b,
>       intervalScaleDomainXform :: Iso.Iso a Double, 
>       intervalScaleRange :: (b, b),
>       intervalScaleDomain :: (a, a),
>       intervalScaleName :: String
>     }

> fundamentalIntervalScale :: IntervalScale Double Double
> fundamentalIntervalScale = IntervalScale 
>     { intervalScaleRangeXform = Iso.Iso id id,
>       intervalScaleDomainXform = Iso.Iso id id,
>       intervalScaleRange = (0.0, 1.0),
>       intervalScaleDomain = (0.0, 1.0),
>       intervalScaleName = "<unnamed>"
>     }

> intervalScaleDomainTransformation :: Iso.Isomorphism f => f a b -> IntervalScale b c -> IntervalScale a c
> intervalScaleDomainTransformation iso_new s = s
>     { intervalScaleDomainXform = d_old `Iso.o` d_new,
>       intervalScaleDomain = (Iso.ap (Iso.inv d_new) old_min, 
>                              Iso.ap (Iso.inv d_new) old_max)
>     } 
>     where
>     d_new = Iso.toIso iso_new
>     d_old = intervalScaleDomainXform s
>     (old_min, old_max) = intervalScaleDomain s

> intervalScaleRangeTransformation :: Iso.Isomorphism f => f b c -> IntervalScale a b -> IntervalScale a c
> intervalScaleRangeTransformation iso_new s = s
>     { intervalScaleRangeXform = d_new `Iso.o` d_old,
>       intervalScaleRange = (Iso.ap d_new old_min, 
>                             Iso.ap d_new old_max)
>     } 
>     where
>     d_new = Iso.toIso iso_new
>     d_old = intervalScaleRangeXform s
>     (old_min, old_max) = intervalScaleRange s

> intervalScaleRename :: String -> IntervalScale a b -> IntervalScale a b
> intervalScaleRename x scale = scale { intervalScaleName = x }

=== Interval Scales are Iso.Isomorphisms

> intervalScaleApply :: IntervalScale a b -> a -> b
> intervalScaleApply scale a = Iso.ap (g `Iso.o` f) a
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
>     r = r_g `Iso.o` d_g `Iso.o` r_f
>     d = d_f
>     IntervalScale r_g d_g br_g bd_g n_g = g
>     IntervalScale r_f d_f br_f bd_f n_f = f
>     n = n_g ++ " . " ++ n_f

> intervalScaleCompose' :: IntervalScale b c -> IntervalScale a b -> IntervalScale a c
> intervalScaleCompose' g f = IntervalScale r d br_g bd_f n where
>     r = r_g
>     d = d_g `Iso.o` r_f `Iso.o` d_f
>     IntervalScale r_g d_g br_g bd_g n_g = g
>     IntervalScale r_f d_f br_f bd_f n_f = f
>     n = n_g ++ " . " ++ n_f

As it says on the tin.

> instance Iso.Function IntervalScale where
>     ap = intervalScaleApply
>     o = intervalScaleCompose
> instance Iso.Isomorphism IntervalScale where
>     inv = intervalScaleInverse

> instance Scale IntervalScale where
>     name        = intervalScaleName
>     rename      = intervalScaleRename
>     domainXform = intervalScaleDomainTransformation
>     rangeXform  = intervalScaleRangeTransformation

--------------------------------------------------------------------------------

=== Affine scales

Affine scales are the equivalent of d3's "linear" scales. "affine"
because they include a translation term.

> type AffineScale = IntervalScale Double Double

> affineScale :: (Double, Double) -> (Double, Double) -> IntervalScale Double Double
> affineScale (from1, from2) (to1, to2) =
>     intervalScaleDomainTransformation domainIso .
>     intervalScaleRangeTransformation rangeIso $ fundamentalIntervalScale
>     where domainIso = Iso.Iso from12ToZero1 zero1ToFrom12
>           rangeIso = Iso.Iso zero1ToTo12 to12ToZero1
>           xyToZero1 x y v = (v - x) / (y - x)
>           zero1ToXY x y v = v * (y - x) + x
>           from12ToZero1 = xyToZero1 from1 from2
>           zero1ToFrom12 = zero1ToXY from1 from2
>           zero1ToTo12 = zero1ToXY to1 to2
>           to12ToZero1 = xyToZero1 to1 to2

> autoAffineScale :: [a] -> A.Attribute a Double -> IntervalScale Double Double
> autoAffineScale rows (A.MkAttribute (selector, name))
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
>           domainIso = Iso.Iso ab ba
>           ab = intervalScaleApply ls
>           ba = intervalScaleApply (intervalScaleInverse ls)
>           ls = affineScale (new_min, new_max) (old_min, old_max)

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
>                            dScaleIso :: Iso.Iso a b,
>                            dScaleName :: String }

FIXME: What do I do about the name in inv?

> instance Iso.Function DScale where
>     ap scale = Iso.ap (dScaleIso scale)
>     scale_2 `o` scale_1 = DScale vals_2 dVal_2 keys_1 dKey_1 (iso_2 `Iso.o` iso_1) (name_2 ++ " . " ++ name_1)
>         where
>         DScale _      _      keys_1 dKey_1 iso_1 name_1 = scale_1
>         DScale vals_2 dVal_2 _      _      iso_2 name_2 = scale_2
> instance Iso.Isomorphism DScale where
>     inv (DScale vals dVal keys dKey iso name) = DScale keys dKey vals dVal (Iso.inv iso) name

> instance Scale DScale where
>     name         = dScaleName
>     rename str f = f { dScaleName = str }
>     domainXform  = discreteScaleDomainTransformation
>     rangeXform   = discreteScaleRangeTransformation

> functionFromListPairs :: Eq a => [a] -> [b] -> b -> a -> b
> functionFromListPairs keys values ifNotFound key =
>     case lookup key (zip keys values) of
>     Nothing -> ifNotFound
>     Just v  -> v

> discreteScale :: (Eq a, Eq b) => [a] -> a -> [b] -> b -> String -> DScale a b
> discreteScale keys dKey vals dVal name =
>     DScale vals dVal keys dKey (Iso.Iso ab ba) name
>     where ab = functionFromListPairs keys vals dVal
>           ba = functionFromListPairs vals keys dKey

> discreteScale' :: Iso.Isomorphism f => [a] -> a -> b -> (f a b) -> String -> DScale a b
> discreteScale' keys dKey dVal valIso name =
>     DScale { dScaleDomain = keys,
>              dScaleDomainDefault = dKey,
>              dScaleRange = map (Iso.ap valIso) keys,
>              dScaleRangeDefault = dVal,
>              dScaleIso = Iso.toIso valIso,
>              dScaleName = name
>              }

> discreteScaleDomainTransformation :: Iso.Isomorphism f => f a b -> DScale b c -> DScale a c
> discreteScaleDomainTransformation isoNew s = s
>     { dScaleDomain = map fInv $ dScaleDomain s,
>       dScaleDomainDefault = fInv $ dScaleDomainDefault s,
>       dScaleIso = dScaleIso s `Iso.o` Iso.toIso isoNew
>     } where fInv = Iso.ap (Iso.inv isoNew)

> discreteScaleRangeTransformation :: Iso.Isomorphism f => f b c -> DScale a b -> DScale a c
> discreteScaleRangeTransformation isoNew s = s
>     { dScaleRange = map f $ dScaleRange s,
>       dScaleRangeDefault = f $ dScaleRangeDefault s,
>       dScaleIso = Iso.toIso isoNew `Iso.o` dScaleIso s
>     } where f = Iso.ap isoNew

> autoDiscreteScale :: (Default b, Ord b) => [a] -> A.Attribute a b -> DScale b Integer
> autoDiscreteScale dataSet attr@(A.MkAttribute (_, name)) = discreteScale' scaleKeys def def scaleIso name
>     where scaleKeys = image dataSet attr
>           scaleValues = take (length scaleKeys) [1..]
>           scaleAB = functionFromListPairs scaleKeys scaleValues def
>           scaleBA = functionFromListPairs scaleValues scaleKeys def
>           scaleIso = Iso.Iso scaleAB scaleBA
>           image :: Ord b => [a] -> A.Attribute a b -> [b]
>           image dataSet (A.MkAttribute (fn, _)) = Data.Set.toList imageSet
>                 where imageList = map fn dataSet
>                       imageSet = Data.Set.fromList imageList

--------------------------------------------------------------------------------
ticks

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

--------------------------------------------------------------------------------

As Jacob points out,

"so i think you can make an Injective a b class pretty easy. it has two methods

 fwd :: a -> b
 rev :: b -> Maybe a

 and two laws:

 rev . fwd = Just
 fmap fw . rev = id"

With this, scales should be Injective and Isomorphic.

--------------------------------------------------------------------------------

> type AffineScaleInContext rowT = (A.Attribute rowT Double,
>                         [rowT] -> A.Attribute rowT Double -> AffineScale)

> type DiscreteScaleInContext rowT b a = (A.Attribute rowT b,
>                               [rowT] -> A.Attribute rowT b -> DScale b (Colour a))

> scaleFromDiscreteScaleInContext :: DiscreteScaleInContext rowT b a -> [rowT] -> DScale b (Colour a)
> scaleFromDiscreteScaleInContext (attr, scale) rows    = scale rows attr

> scaleFromAffineScaleInContext :: AffineScaleInContext rowT -> [rowT] -> AffineScale
> scaleFromAffineScaleInContext   (attr, scale) rows = scale rows attr
