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
f *-> (x1, x2, x3) = (x1*f,x2*f,x3*f)

maxF :: Double -> (Double,Double,Double) -> (Double,Double,Double)
maxF f (x,y,z) = (max x f, max y f, max z f)

minF :: Double -> (Double,Double,Double) -> (Double,Double,Double)
minF f (x,y,z) = (min x f, min y f, min z f)

(*.) :: Vector -> Vector -> Double
(x1,x2,x3) *. (y1,y2,y3) = x1*y1 + x2*y2 + x3*y3

len :: Vector -> Double
len v = sqrt (v *. v)

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