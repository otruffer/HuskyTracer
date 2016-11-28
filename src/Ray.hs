{-# LANGUAGE OverloadedStrings #-}

module Ray where

import Linear
import Codec.Picture
import Data.Word


data HitRecord = HitRecord Double Point3D Vector deriving Show-- distance from camera, hitpoint in space, normal vector

instance Ord HitRecord where
    (HitRecord t1 _ _) `compare` (HitRecord t2 _ _) = t1 `compare` t2

instance Eq HitRecord where
    (HitRecord t1 _ _) == (HitRecord t2 _ _) = t1 == t2

hitsSphere :: Ray -> Sphere -> Maybe HitRecord
hitsSphere (Ray rayOrigin rayDirection) (Sphere sphereOrigin sphereRadius)
    | discriminant >= 0 = Just (calculateHitRecord a b c rayOrigin rayDirection sphereOrigin)
    | otherwise = Nothing
        where   oc = rayOrigin <-> sphereOrigin
                a = rayDirection *. rayDirection
                b = 2 * rayDirection *. oc
                c = oc *. oc - (sphereRadius * sphereRadius)
                discriminant = b * b - 4 * a * c

calculateHitRecord :: Double -> Double -> Double -> Point3D -> Vector -> Point3D -> HitRecord
calculateHitRecord a b c origin direction sphereOrigin = HitRecord t hitPoint normal
    where   t1 = calculateT1 a b c
            t2 = calculateT2 a b c
            t = min t1 t2
            hitPoint = origin <+> (t *-> direction)
            normal = norm $ hitPoint <-> sphereOrigin

calculateT1 a b c = (-b - sqrt(b * b - 4 * a * c)) / (2 * a)
calculateT2 a b c = (-b + sqrt(b * b - 4 * a * c)) / (2 * a)

aSphere :: Sphere
aSphere = Sphere (0.0, 0.0, -1.0) 0.5

cameraOrigin :: Point3D
cameraOrigin = (0.0, 0.0, 0.0)

lowerLeft :: Vector
lowerLeft = (-2.0, -2.0, -1.0)

horizontal :: Vector
horizontal = (4.0, 0, 0)

sphereColor :: (Word8, Word8, Word8)
sphereColor = (255, 0, 0)

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
            direction = norm $ lowerLeft <+> ((percentage x mapWidth) *-> horizontal) <+> ((percentage y mapHeight) *-> vertical)

trace :: Ray -> (Word8, Word8, Word8)
trace ray@(Ray origin direction) = calculateColor (hitsSphere ray aSphere) direction

calculateColor :: Maybe HitRecord -> Vector -> (Word8, Word8, Word8)
calculateColor (Just hitRecord) direction = calculateHitColor hitRecord direction
calculateColor Nothing _ = (0, 0, 0)

calculateHitColor (HitRecord _ _ normal) direction = abs (normal *. direction) *=> sphereColor

percentage :: Int -> Int -> Double
percentage x total = (fromIntegral x) / (fromIntegral total)

createImage :: IO ()
createImage = do
    putStrLn "generating image"
    let image = generateMap
    savePngImage "ciao.png" (ImageRGB8 image)
