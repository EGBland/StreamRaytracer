module Raylude (
    VecType, Vector,
    Point, Colour, Direction, Ray
)
where

import Vector3

type VecType = Double
type Vector = Vec3 VecType
type Ray = Ray3 VecType

type Point = Vector
type Colour = Vector
type Direction = Vector