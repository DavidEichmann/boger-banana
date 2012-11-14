module BB.Util.Vec (
        scale
) where

type Vec3 = (Float, Float, Float)

apply :: (Float -> Float) -> Vec3 -> Vec3
apply fn (x,y,z) = (fn x, fn y, fn z)

scale :: Float -> Vec3 -> Vec3
scale s = apply (*s)