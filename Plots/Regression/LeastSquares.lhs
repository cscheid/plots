> module Plots.Regression.LeastSquares(
> -- heh, it'd be nice if I could get slope and intercept directly from these.
>   leastSquaresWithFunctionSpace,
>   linearLeastSquares
> ) where


> import Numeric.Container
> import Numeric.LinearAlgebra.Algorithms

> leastSquaresWithFunctionSpace :: (Floating a, Field a) => [a -> a] -> [(a, a)] -> a -> a
> leastSquaresWithFunctionSpace basis pts newX =
>      newXMat `multiply` coefs `atIndex` (0,0)
>   where
>   -- wow newXMat is ugly
>   newXMat   = ((1 >< sz) $ concatMap toList (phixs (fromList [newX])))
>   x         = fromList $ map fst pts
>   y         = fromList $ map snd pts
>   phixs x   = map (flip cmap x) basis
>   fxs       = phixs x
>   innerYfxs = (sz >< 1) $ map (dot y) fxs
>   sz        = length basis
>   matA      = (sz >< sz) $ map (uncurry dot) $ crossProduct fxs fxs
>   coefs     = linearSolveSVD matA innerYfxs

> linearLeastSquares = leastSquaresWithFunctionSpace [const (1.0 :: Double), id]

> crossProduct :: [a] -> [b] -> [(a, b)]
> crossProduct xs ys =
>     do x <- xs
>        y <- ys
>        return (x, y)
