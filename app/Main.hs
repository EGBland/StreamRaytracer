{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

import Codec.Picture (generateImage, savePngImage, PixelRGB8(..), Pixel8, Image, DynamicImage(ImageRGB8))
import qualified Data.Bifunctor as BiF
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Text.Printf (printf)

import Raylude
import Vector3 hiding (x,y,z)
import qualified Vector3 as V
import World
import Vector3 (x)


cameraOrigin = Vec3 0 0 0
focalLength = 1
imageWidth = 800 :: Int
imageHeight = 600 :: Int
aspectRatio = fromIntegral imageWidth / fromIntegral imageHeight :: Double

viewportHeight = 2 :: Double
viewportWidth = viewportHeight * aspectRatio :: Double


world = [Sphere (Vec3 0 0 (-2)) 1, Sphere (Vec3 0 (-100.5) (-2)) 100] :: [Sphere]

raytrace :: Ray -> Colour
raytrace ray@(Ray3 origin direction) =
    let
        bg  = gradient (Vec3 0 0 1) (Vec3 1 1 1) ((1+V.y direction) / 2)
        hitPoints = mapMaybe (hits ray) world
    in case hitPoints of
        [] -> bg
        _  -> let
                (_,nml) = minimumBy (comparing $ mag . fst) hitPoints
            in (1/2) £** (pure 1 £+ nml) -- TODO rewrite. ugly :(


colourToPixel :: Colour -> PixelRGB8
colourToPixel colour =
    let
        pxVec = toEnum . floor <$> (255 £** colour) :: Vec3 Pixel8
    in
        PixelRGB8 (V.x pxVec) (V.y pxVec) (V.z pxVec)

colourAt :: Int -> Int -> PixelRGB8
colourAt x y =
    let
        botLeftCorner = cameraOrigin £- Vec3 (viewportWidth / 2) (-viewportHeight / 2) focalLength
        onePixelWidth = viewportWidth / fromIntegral imageWidth
        onePixelHeight = viewportHeight / fromIntegral imageHeight
        rayDirection = normalise $ botLeftCorner £+ Vec3 (fromIntegral x * onePixelWidth) (-fromIntegral y * onePixelHeight) 0
        ray = Ray3 cameraOrigin rayDirection
    in
        colourToPixel $ raytrace ray



raytraceImage :: DynamicImage
raytraceImage = ImageRGB8 $ generateImage colourAt imageWidth imageHeight

main :: IO ()
main = do
    savePngImage "./result.png" raytraceImage
--main = putStrLn . toPPM . map (uncurry colourAt) $ [(x,y) | x <- [0..imageWidth-1], y <- [0..imageHeight-1]]
