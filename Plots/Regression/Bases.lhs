> module Plots.Regression.Bases (
>   monomial, legendre
> ) where

> monomial :: (Integral a, Floating b) => a -> b -> b
> monomial k x = x ** (fromIntegral k)

--------------------------------------------------------------------------------
orthogonal function spaces

> legendre :: (Integral a, Floating b) => a -> b -> b
> legendre 0 _ = 1
> legendre 1 x = x
> legendre k _
>     | k < 0 = error "k must be positive"
> legendre k x = helper 1 x 2
>     where
>     helper nMinus2 nMinus1 deg
>       | k == deg  = n
>       | otherwise = helper nMinus1 n (deg + 1)
>       where 
>         n = ((2 * d - 1) * x * nMinus1 - (d - 1) * nMinus2) / d
>         d = fromIntegral deg
