{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Material (
    Material, attenuate, bounce, pack,
    Diffuse(..)
)
where

import qualified Data.Bifunctor as BiF
import System.Random (RandomGen)

import Raylude
import Vector3 hiding (x,y,z)

class MaterialT a where
    attenuate :: a -> Colour
    bounce :: (RandomGen g) => a -> g -> Vector -> (Vector,g)

data Material = forall a. MaterialT a => Material a

instance MaterialT Material where
    attenuate (Material a) = attenuate a
    bounce (Material a) = bounce a

pack :: (MaterialT a) => a -> Material
pack = Material


data Diffuse = Diffuse Colour

instance MaterialT Diffuse where
    attenuate (Diffuse clr) = clr
    bounce (Diffuse clr) g normal =
        let
            (offset,g2) = BiF.first (0.999£**) $ randomUnit g
        in
            (normalise $ normal £+ offset,g2)