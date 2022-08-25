{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

import Codec.Picture (generateFoldImage, savePngImage, PixelRGB8(..), Pixel8, DynamicImage(ImageRGB8))
import qualified Data.Bifunctor as BiF
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Tuple (swap)
import System.Random (RandomGen, mkStdGen, uniformR)

import Material hiding (pack)
import qualified Material as M
import Raylude
import Vector3 hiding (x,y,z)
import qualified Vector3 as V
import World

type Point2D = (VecType, VecType)

cameraOrigin = Vec3 0 0 0       -- where the camera is
focalLength = 1                 -- how far the viewport is from the camera
imageWidth = 400 :: Int         -- width of the generated image
imageHeight = 300 :: Int        -- height of the generated image
aspectRatio = fromIntegral imageWidth / fromIntegral imageHeight :: Double
viewportHeight = 2 :: Double    -- height of the viewport
viewportWidth = viewportHeight * aspectRatio :: Double

rngSeed = 1122 :: Int
rng = mkStdGen rngSeed

msaaNoSamples = 100 :: Int  -- number of samples to take when doing multisampling

myMaterial = M.pack $ Diffuse (Vec3 0.75 0.75 0.75)
world = [Sphere myMaterial (Vec3 0 0 (-2)) 1, Sphere myMaterial (Vec3 0 (-100.5) (-2)) 100] :: [Sphere]


-- Perform gamma correction on a colour.
gamma :: VecType -> Colour -> Colour
gamma g = fmap (**(1/g))

-- Trace the given ray into the world.
raytrace :: (RandomGen g) => g -> Ray -> (Colour,g)
raytrace = raytrace' 0

-- Does the actual ray tracing; returns black after 20 bounces.
raytrace' :: (RandomGen g) => Int -> g -> Ray -> (Colour,g)
raytrace' 20 g _ = (Vec3 0 0 0, g)
raytrace' n g ray@(Ray3 _ direction) =
    let
        bg  = gradient (Vec3 0 0 1) (Vec3 1 1 1) ((1+V.y direction) / 2)
        hitPoints = mapMaybe (hits g ray) world
    in case hitPoints of
        [] -> (bg,g)
        _  -> let
                (r2o,r2d,clr,g2) = minimumBy (comparing $ mag . \(x,_,_,_) -> x) hitPoints -- TODO this needs redoing to consider bounces
            in BiF.first (clr£*) $ raytrace' (n+1) g2 (Ray3 r2o r2d)


-- Map a Vector colour to a JuicyPixels PixelRGB8.
colourToPixel :: Colour -> PixelRGB8
colourToPixel colour =
    let
        pxVec = toEnum . floor <$> (255 £** colour) :: Vec3 Pixel8
    in
        PixelRGB8 (V.x pxVec) (V.y pxVec) (V.z pxVec)

-- Convert pixel-coordinates (a position on the image) to a ray from the camera origin through the viewport.
getRay :: Point2D -> Ray
getRay (x,y) =
    let
        botLeftCorner = cameraOrigin £- Vec3 (viewportWidth / 2) (-viewportHeight / 2) focalLength
        onePixelWidth = viewportWidth / fromIntegral imageWidth
        onePixelHeight = viewportHeight / fromIntegral imageHeight
        rayDirection = normalise $ botLeftCorner £+ Vec3 (x * onePixelWidth) (-y * onePixelHeight) 0
    in
        Ray3 cameraOrigin rayDirection

-- A fold that generates a random jitter, then traces a jittered ray into the world.
generateSamplesFold :: (RandomGen g) => Point2D -> a -> ([Colour],g) -> ([Colour],g)
generateSamplesFold pt _ (lst,g) =
    let
        (jit,g2) = jitter pt g
        (clr,g3) = raytrace g2 . getRay $ jit

    in
        (clr:lst, g3)

-- Get the colour at the given pixel-coordinates.
colourAt :: (RandomGen g) => g -> Int -> Int -> (g,PixelRGB8)
colourAt g x y =
    let
        (samples,gn) = foldr (generateSamplesFold (fromIntegral x, fromIntegral y)) ([],g) [1..msaaNoSamples]
        sumColour = foldr1 (£+) samples
        avgColour = (1 / fromIntegral msaaNoSamples) £** sumColour
    in
        (gn,colourToPixel . gamma 2 $ avgColour)

-- Get a small (0<=x,y<=1) jitter for the given Point2D.
jitter :: (RandomGen g) => Point2D -> g -> (Point2D, g)
jitter (x,y) g =
    let
        (jx,g2) = uniformR (0::VecType,1::VecType) g
        (jy,g3) = uniformR (0::VecType,1::VecType) g2
    in
        ((x+jx,y+jy),g3)

-- The ray tracing function that ties everything together.
raytraceImage :: (RandomGen g) => g -> (DynamicImage,g)
--raytraceImage = ImageRGB8 $ generateImage colourAt imageWidth imageHeight
raytraceImage g = swap . BiF.second ImageRGB8 $ generateFoldImage colourAt g imageWidth imageHeight


main :: IO ()
main = savePngImage "./result.png" $ fst $ raytraceImage rng
