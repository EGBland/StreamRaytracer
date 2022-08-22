module Raylude (
    VecType, Vector,
    Point, Colour, Direction, Ray,
    randomUnit
)
where

import System.Random (RandomGen, uniformR)
import Vector3

type VecType = Double
type Vector = Vec3 VecType
type Ray = Ray3 VecType

type Point = Vector
type Colour = Vector
type Direction = Vector

randomUnit :: (RandomGen g) => g -> (Vector, g)
randomUnit g =
    let
        (x, g2) = uniformR (-1::VecType,1::VecType) g
        (y, g3) = uniformR (-1::VecType,1::VecType) g2
        (z, g4) = uniformR (-1::VecType,1::VecType) g3
    in
        (normalise $ Vec3 x y z, g4)