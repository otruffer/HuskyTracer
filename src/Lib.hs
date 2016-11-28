{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Codec.Picture
import Data.Word
import Linear.Vector

type Point2D = (Int, Int)
type Point3D = (Double, Double, Double)
type Vector = (Double, Double, Double)
type Radius = Double
data Ray = Ray Point3D Vector
data Sphere = Sphere Point3D Radius deriving Show
data HitRecord = HitRecord Double Point3D Vector

instance Ord HitRecord where
    (HitRecord t1 _ _) `compare` (HitRecord t2 _ _) = t1 `compare` t2

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

hitsSphere :: Ray -> Sphere -> Maybe HitRecord
hitsSphere (Ray rayOrigin rayDirection) (Sphere sphereOrigin sphereRadius)
    | discriminant >= 0 = Just (calculateHitRecord a b c)
    | otherwise = Nothing
        where   oc = rayOrigin <-> sphereOrigin
                a = rayDirection *. rayDirection
                b = 2 * rayDirection *. oc
                c = oc *. oc - (sphereRadius * sphereRadius)
                discriminant = b * b - 4 * a * c

calculateHitRecord a b c = min hit1 hit2
    where   t1 = calculateT1 a b c
            t2 = calculateT2 a b c
            hit1 = HitRecord

calculateT1 a b c = (-b - sqrt(b * b - a * c)) / a
calculateT2 a b c = (-b + sqrt(b * b - a * c)) / a

aSphere :: Sphere
aSphere = Sphere (0.0, 0.0, -1.0) 0.5

cameraOrigin :: Point3D
cameraOrigin = (0.0, 0.0, 0.0)

lowerLeft :: Vector
lowerLeft = (-2.0, -2.0, -1.0)

horizontal :: Vector
horizontal = (4.0, 0, 0)

vertical :: Vector
vertical = (0.0, 4.0, 0.0)

mapHeight = 400
mapWidth = 400

generateMap :: Image PixelRGB8
generateMap = generateImage f mapHeight mapWidth
                where   f x y = getPixelRGB8 x y

getPixelRGB8 x y = PixelRGB8 r g b
    where  (r, g, b) = rayTrace x y

rayTrace :: Int -> Int -> (Word8, Word8, Word8)
rayTrace x y = trace (Ray origin direction)
    where   origin = cameraOrigin
            direction = lowerLeft <+> ((percentage x mapWidth) *-> horizontal) <+> ((percentage y mapHeight) *-> vertical)

trace :: Ray -> (Word8, Word8, Word8)
trace ray = calculateColor $ hitsSphere ray aSphere

calculateColor :: Maybe HitRecord -> (Word8, Word8, Word8)
calculateColor (Just hitRecord) = (255, 0, 0)
calculateColor Nothing = (0, 0, 0)

percentage :: Int -> Int -> Double
percentage x total = (fromIntegral x) / (fromIntegral total)

someFunc :: IO ()
someFunc = do
    putStrLn "generating image"
    let image = generateMap
    savePngImage "ciao.png" (ImageRGB8 image)
