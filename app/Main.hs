module Main where

import Text.Printf (printf)

data Vec3 = Vec3 Double Double Double

-- componentwise addition
(&+) :: Vec3 -> Vec3 -> Vec3
infixl 6 &+
(Vec3 x1 y1 z1) &+ (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

-- componentwise subtraction
(&-) :: Vec3 -> Vec3 -> Vec3
infixl 6 &-
(Vec3 x1 y1 z1) &- (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)

-- componentwise multiplication
(&*) :: Vec3 -> Vec3 -> Vec3
infixl 7 &*
(Vec3 x1 y1 z1) &* (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)

-- scalar multiplication
(&**) :: Double -> Vec3 -> Vec3
infixl 7 &**
t &** (Vec3 x y z) = Vec3 (t*x) (t*y) (t*z)

mag :: Vec3 -> Double
mag (Vec3 x y z) = sqrt $ x*x + y*y + z*z

normalise :: Vec3 -> Vec3
normalise v = let len = mag v in (1 / len) &** v

type Point = Vec3
type Colour = Vec3
type Direction = Vec3
type Ray = (Point,Direction)

(@@) :: Ray -> Double -> Point
(origin,direction) @@ t = origin &+ t &** direction


cameraOrigin = Vec3 0 0 0 :: Vec3
focalLength = 1 :: Double
imageWidth = 800 :: Int
imageHeight = 600 :: Int
aspectRatio = (fromIntegral imageWidth) / (fromIntegral imageHeight) :: Double

viewportHeight = 2 :: Double
viewportWidth = viewportHeight * aspectRatio :: Double

raytrace :: Ray -> Vec3
raytrace (origin,direction) = Vec3 0 0 1

colourAt :: Int -> Int -> Vec3
colourAt x y =
    let
        topLeftCorner = cameraOrigin &- Vec3 (viewportWidth / 2) (viewportHeight / 2) focalLength
        onePixelWidth = viewportWidth / fromIntegral imageWidth
        onePixelHeight = viewportHeight / fromIntegral imageHeight
        rayDirection = normalise $ topLeftCorner &+ Vec3 (fromIntegral x * onePixelWidth) (fromIntegral y * onePixelHeight) 0
        ray = (cameraOrigin, rayDirection)
    in
        raytrace ray


toPPMPixel :: Colour -> String
toPPMPixel (Vec3 r g b) = printf "%d\t%d\t%d\n" ((floor $ 255*r) :: Int) ((floor $ 255*g) :: Int) ((floor $ 255*b) :: Int)

toPPM :: [Colour] -> String
toPPM pxs = printf "P3\n%d\t%d\n255\n" imageWidth imageHeight ++ concatMap toPPMPixel pxs

main :: IO ()
main = putStrLn . toPPM . map (uncurry colourAt) $ [(x,y) | x <-[0..imageWidth-1], y <- [0..imageHeight-1]]