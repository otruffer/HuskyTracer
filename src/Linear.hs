{-# LANGUAGE OverloadedStrings #-}

module Linear where

import Data.Word

type Point2D = (Int, Int)
type Point3D = (Double, Double, Double)
type Vector = (Double, Double, Double)
type Radius = Double
data Ray = Ray Point3D Vector
data Sphere = Sphere Point3D Radius deriving Show

type Color = (Word8, Word8, Word8)


(<+>) :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
(x1, x2, x3) <+> (y1, y2, y3) = (x1+y1, x2+y2, x3+y3)

(<->) :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
(x1, x2, x3) <-> (y1, y2, y3) = (x1-y1, x2-y2, x3-y3)

(<*>) :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
(x1, x2, x3) <*> (y1, y2, y3) = (x1*y1, x2*y2, x3*y3)

(*->) :: Double -> (Double,Double,Double) -> (Double,Double,Double)
k *-> x = mapTuple (\xi -> k * xi) x

maxF :: Double -> (Double,Double,Double) -> (Double,Double,Double)
maxF f (x,y,z) = (max x f, max y f, max z f)

minF :: Double -> (Double,Double,Double) -> (Double,Double,Double)
minF f (x,y,z) = (min x f, min y f, min z f)

(*.) :: Vector -> Vector -> Double
(x1,x2,x3) *. (y1,y2,y3) = x1*y1 + x2*y2 + x3*y3

len :: Vector -> Double
len v = sqrt (v *. v)

getY :: Vector -> Double
getY (_, y , _) = y

norm :: Vector -> Vector
norm v
 | len v < 10**(-9) = (0.0,0.0,0.0)
 | otherwise = (1/(len v)) *-> v

mkNormVect :: Point3D -> Point3D -> Vector
mkNormVect v w = norm (w <-> v)

dist :: Point3D -> Point3D -> Double
dist p0 p1 = sqrt ((p1 <-> p0) *. (p1 <-> p0))

clip :: (Double,Double,Double) -> (Double,Double,Double)
clip = (maxF 0.0) . (minF 1.0)

-- Todo: separate

(*=>) :: Double -> Color -> Color
f *=> (x1, x2, x3) = (round $ fromIntegral x1 * f, round $ fromIntegral x2 * f, round $ fromIntegral x3 * f)

interpolateColor :: Double -> Color -> Color -> Color
interpolateColor t (r1, g1, b1) (r2, g2, b2) = (interpolateWord t r1 r2, interpolateWord t g1 g2, interpolateWord t b1 b2)

interpolateWord :: Double -> Word8 -> Word8 -> Word8
interpolateWord t a b = round $ (fromIntegral a * t) + (fromIntegral b * (1.0 - t))

mapTuple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTuple f (a1, a2, a3) = (f a1, f a2, f a3)

batch :: (a -> b -> c) -> (a, a, a) -> (b, b, b) -> (c, c, c)
batch f (x1, x2, x3) (y1, y2, y3) = (f x1 y1, f x2 y2, f x3 y3)