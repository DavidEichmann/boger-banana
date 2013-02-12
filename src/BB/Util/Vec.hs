module BB.Util.Vec (
        Vec3,
        scale,
        add,
        to,
        dot,
        norm,
        dist,
        sqrNorm,
        sqrDist,
        unit,
        vecSum
) where


type Vec3 = (Float, Float, Float)

-- helper

apply :: (Float -> Float) -> Vec3 -> Vec3
apply fn (x,y,z) = (fn x, fn y, fn z)

impose :: (Float -> Float -> Float) -> Vec3 -> Vec3 -> Vec3
impose fn (x1,y1,z1) (x2,y2,z2) = (fn x1 x2, fn y1 y2, fn z1 z2)

sum3 :: Vec3 -> Float
sum3 (a,b,c) = sum [a,b,c] 

zero :: Vec3
zero = (0,0,0)

-- exported

scale :: Float -> Vec3 -> Vec3
scale s = apply (*s)

add :: Vec3 -> Vec3 -> Vec3
add = impose (+)

vecSum :: [Vec3] -> Vec3
vecSum = foldr add zero

to :: Vec3 -> Vec3 -> Vec3
to a b = impose (-) b a

dot :: Vec3 -> Vec3 -> Float
dot a b = sum3 (impose (*) a b)

sqrDist :: Vec3 -> Vec3 -> Float
sqrDist a b = sqrNorm (a `to` b)

dist :: Vec3 -> Vec3 -> Float
dist a b = sqrt (sqrDist a b)

sqrNorm :: Vec3 -> Float
sqrNorm v = v `dot` v

norm :: Vec3 -> Float
norm = sqrt . sqrNorm 

unit :: Vec3 -> Vec3
unit v = scale (1/(norm v)) v

        
        
        
        