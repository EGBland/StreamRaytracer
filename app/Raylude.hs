module Raylude (
    VecType, Vector,
    Point, Colour, Direction, Ray,
    randomUnit
)
where

import System.Random (RandomGen, uniformR)
import Vector3 hiding (x,y,z)

type VecType = Double
type Vector = Vec3 VecType
type Ray = Ray3 VecType

type Point = Vector
type Colour = Vector
type Direction = Vector

randomUnit :: (RandomGen g) => g -> (Vector, g)
randomUnit g =
    let
        (theta, g2) = uniformR (0::VecType,pi::VecType) g
        (phi, g3) = uniformR (0::VecType,2*pi::VecType) g2
        x = cos phi * sin theta
        y = sin phi * sin theta
        z = cos theta
    in
        (normalise $ Vec3 x y z, g3)