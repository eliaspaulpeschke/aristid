module Util where

import Linear (V3(V3))

copysign :: (Ord a, Num a) => a -> a -> a
copysign a b = if b >= 0 then abs a else (-1) * abs a

perp :: V3 Float -> V3 Float
perp (V3 x y z) = V3 nx ny nz
        where
        nx = copysign z x
        ny = copysign z y
        nz = -(copysign x z) - copysign y z 


