{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE ExistentialQuantification #-}
module World (
    hits, normal,
    Sphere(..)
)
where

import Data.Maybe (fromJust)
import System.Random (RandomGen)

import Material
import Raylude
import Vector3 hiding (x,y,z)


class Hittable a where
    hits     :: (RandomGen g) => g -> Ray -> a -> Maybe (Point,Direction,Colour,g)
    normal   :: Point -> a -> Direction
    material :: a -> Material


data Sphere = Sphere Material Point VecType


instance Hittable Sphere where
    hits g ray@(Ray3 origin direction) (Sphere mat centre radius) = -- TODO rewrite this, it's fucking awful!!!!
        let
            a = direction £. direction
            b = 2*(direction £. (origin £- centre))
            c = (origin £- centre) £. (origin £- centre) - radius*radius
            discriminant = b*b - 4*a*c
            hitT = (-b - sqrt discriminant) / (2*a)
            hitPoint = if discriminant < 0 || hitT < 0.001 then Nothing else Just $ ray @@ hitT
            scatter = bounce mat g <$> hitPoint
            colour = attenuate mat
        in
            case scatter of
                Nothing -> Nothing
                Just (scatterDir,g2) -> Just (fromJust hitPoint,scatterDir,colour,g2)
    
    p `normal` (Sphere _ centre _) = normalise $ p £- centre

    material (Sphere mat _ _) = mat