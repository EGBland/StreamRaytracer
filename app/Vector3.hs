{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Vector3 (
    Vec3(..), Ray3(..),
    (£+), (£-), (£*), (£**), (£.), (££), mag, normalise,
    x, y, z,
    gradient,
    (@@)
)
where


import Control.Applicative (liftA2)

data Vec3 a = Vec3 a a a
    deriving (Show)
data Ray3 a = Ray3 (Vec3 a) (Vec3 a)
    deriving (Show)

instance Functor Vec3 where
    fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Applicative Vec3 where
    pure a = Vec3 a a a
    liftA2 f (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (f x1 x2) (f y1 y2) (f z1 z2)

instance Foldable Vec3 where
    foldr accf x0 (Vec3 x y z) = x `accf` (y `accf` (z `accf` x0))

-- componentwise addition
(£+) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
infixl 6 £+
(£+) = liftA2 (+)

-- componentwise subtraction
(£-) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
infixl 6 £-
(£-) = liftA2 (-)

-- componentwise multiplication
(£*) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
infixl 7 £*
(£*) = liftA2 (*)

-- scalar multiplication
(£**) :: (Num a) => a -> Vec3 a -> Vec3 a
infixl 7 £**
t £** v = pure t £* v

-- dot product
(£.) :: (Num a) => Vec3 a -> Vec3 a -> a
u £. v = sum $ u £* v

-- cross product
(££) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
(Vec3 x1 y1 z1) ££ (Vec3 x2 y2 z2) = Vec3 (y1*z2-y2*z1) (x2*z1-x1*z2) (x1*y2-x2*y1)

-- vector magnitude
mag :: (Floating a) => Vec3 a -> a
mag v = sqrt $ v £. v

-- normalise a vector to have length 1
normalise :: (Floating a) => Vec3 a -> Vec3 a
normalise v = let len = mag v in (1 / len) £** v


x :: Vec3 a -> a
x (Vec3 a _ _) = a

y :: Vec3 a -> a
y (Vec3 _ b _) = b

z :: Vec3 a -> a
z (Vec3 _ _ c) = c


gradient :: (Num a) => Vec3 a -> Vec3 a -> a -> Vec3 a
gradient gLeft gRight t = (1-t) £** gLeft £+ t £** gRight


(@@) :: (Num a) => Ray3 a -> a -> Vec3 a
(Ray3 origin direction) @@ t = origin £+ t £** direction