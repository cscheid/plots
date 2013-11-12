> module Plots.DiagramUtils where

> import Diagrams.Backend.SVG
> import Diagrams.Prelude
> import System.IO.Unsafe

> type DC = Diagram SVG R2

> translated :: DC -> (Double, Double) -> DC
> translated f (x, y) = f # translate (r2 (x, y))

--------------------------------------------------------------------------------
stacking combinators with different origins

|||> is like |||, but the origin is b's origin, not a's origin

> (|||>) :: DC -> DC -> DC
> a |||> b = beside (r2 (-1.0, 0.0)) b a

> instance Alignable a => Alignable (b -> a) where
>     alignBy v d f b = alignBy v d (f b)

> instance Juxtaposable a => Juxtaposable (b -> a) where
>     juxtapose v f1 f2 b = juxtapose v (f1 b) (f2 b)

> safeFromJust Nothing = mempty
> safeFromJust (Just x) = x

--------------------------------------------------------------------------------

> printing :: Show a => a -> a
> printing a = (unsafePerformIO $ print a) `seq` a

> printThen :: Show a => a -> b -> b
> printThen a b = (unsafePerformIO $ print a) `seq` b
