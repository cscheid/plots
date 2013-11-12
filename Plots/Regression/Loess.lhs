-- > module Plots.Regression.Loess(
-- > ) where

> legendrePolynomial :: (Integral a, Floating b) => a -> b -> b
> legendrePolynomial 0 _ = 1
> legendrePolynomial 1 x = x
> legendrePolynomial k _
>     | k < 0 = error "k must be positive"
> legendrePolynomial k x = helper 1 x 2
>     where
>     helper nMinus2 nMinus1 deg
>       | k == deg  = n
>       | otherwise = helper nMinus1 n (deg + 1)
>       where 
>         n = ((2 * d - 1) * x * nMinus1 - (d - 1) * nMinus2) / d
>         d = fromIntegral deg
