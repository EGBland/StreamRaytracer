module World (
    hits, normal,
    Sphere(..)
)
where

import Raylude
import Vector3 hiding (x,y,z)


class Hittable a where
    hits   :: Ray -> a -> Maybe (Point,Direction)
    normal :: Point -> a -> Direction


data Sphere = Sphere Point VecType
    deriving (Show)


instance Hittable Sphere where
    ray@(Ray3 origin direction) `hits` sph@(Sphere centre radius) =
        let
            a = direction £. direction
            b = 2*(direction £. (origin £- centre))
            c = (origin £- centre) £. (origin £- centre) - radius*radius
            discriminant = b*b - 4*a*c
            hitT = (-b - sqrt discriminant) / (2*a)
            hitPoint = ray @@ hitT
        in
            if discriminant < 0 || hitT < 0 then Nothing else Just (hitPoint,hitPoint `normal` sph)
    
    p `normal` (Sphere centre _) = normalise $ p £- centre