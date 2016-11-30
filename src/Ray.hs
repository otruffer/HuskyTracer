{-# LANGUAGE OverloadedStrings #-}

module Ray where

import Linear
import Codec.Picture
import Data.Word
import Data.Maybe


data HitRecord = HitRecord Double Point3D Vector deriving Show-- distance from camera, hitpoint in space, normal vector
data Material = Matte Color | Reflection
data Ball = Ball Sphere Material
data MaterialHit = MaterialHit HitRecord Material

instance Ord HitRecord where
    (HitRecord t1 _ _) `compare` (HitRecord t2 _ _) = t1 `compare` t2

instance Eq HitRecord where
    (HitRecord t1 _ _) == (HitRecord t2 _ _) = t1 == t2

instance Ord MaterialHit where
    (MaterialHit hitRecord1 _ ) `compare` (MaterialHit hitRecord2 _ ) = hitRecord1 `compare` hitRecord2

instance Eq MaterialHit where
    (MaterialHit hitRecord1 _ ) == (MaterialHit hitRecord2 _ ) = hitRecord1 == hitRecord2

hitsSphere :: Ray -> Sphere -> Maybe HitRecord
hitsSphere (Ray rayOrigin rayDirection) (Sphere sphereOrigin sphereRadius)
    | discriminant > 0 = calculateHitRecord a b c rayOrigin rayDirection sphereOrigin
    | otherwise = Nothing
        where   oc = rayOrigin <-> sphereOrigin
                a = rayDirection *. rayDirection
                b = 2 * rayDirection *. oc
                c = oc *. oc - (sphereRadius * sphereRadius)
                discriminant = b * b - 4 * a * c

calculateHitRecord :: Double -> Double -> Double -> Point3D -> Vector -> Point3D -> Maybe HitRecord
calculateHitRecord a b c origin direction sphereOrigin = buildHitRecord t origin direction sphereOrigin
    where   t1 = onlyPositive $ calculateT1 a b c
            t2 = onlyPositive $ calculateT2 a b c
            t = maybeApply min t1 t2

-- We only want positive
onlyPositive :: Double -> Maybe Double
onlyPositive t
    | t > 10 ** (-9) = Just t
    | otherwise = Nothing

calculateT1 :: Double -> Double -> Double -> Double
calculateT1 a b c = (-b - sqrt(b * b - 4 * a * c)) / (2 * a)

calculateT2 :: Double -> Double -> Double -> Double
calculateT2 a b c = (-b + sqrt(b * b - 4 * a * c)) / (2 * a)

buildHitRecord :: Maybe Double -> Point3D -> Vector -> Point3D -> Maybe HitRecord
buildHitRecord Nothing _ _ _ = Nothing
buildHitRecord (Just t) origin direction sphereOrigin = Just (HitRecord t hitPoint normal)
    where   hitPoint = origin <+> (t *-> direction)
            normal = norm $ hitPoint <-> sphereOrigin

-- if
maybeApply :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybeApply _ Nothing Nothing = Nothing
maybeApply _ (Just x) Nothing = (Just x)
maybeApply _ Nothing (Just y) = (Just y)
maybeApply f (Just x) (Just y) = Just (f x y)

sphere1 :: Ball
sphere1 = Ball (Sphere (0.0, 0.0, -5.0) 0.5) redMatte

sphere2 :: Ball
sphere2 = Ball (Sphere (0.0, 30.5, -5.0) 30) greenMatte

sphere3 :: Ball
sphere3 = Ball (Sphere (1.0, -1.0, -10.0) 0.5) Reflection

sphere4 :: Ball
sphere4 = Ball (Sphere (-0.5, -2.0, -5.0) 0.5) redMatte

sphere5 = Ball (Sphere (1.0, 0.0, -6.0) 0.5) Reflection

sphere6 = Ball (Sphere (-1.0, 0.0, -4.0) 0.5) Reflection

sphere7 :: Ball
sphere7 = Ball (Sphere (-1.0, -1.0, -10.0) 0.5) blueMatte

balls = [sphere1, sphere2, sphere3, sphere4, sphere5, sphere6, sphere7]

cameraOrigin :: Point3D
cameraOrigin = (0.0, -0.25, 0.0)
--cameraOrigin = (0.0, -40.0, 0.0)

cameraDirection :: Vector
cameraDirection = norm (0.0, 0.0, -1.0)

lowerLeft :: Vector
lowerLeft = (-0.5, -0.5, 0.0)
--lowerLeft = norm (-0.5, 0.25, -0.25)

horizontal :: Vector
horizontal = (1.0, 0, 0)

sphereColor :: (Word8, Word8, Word8)
sphereColor = (155, 70, 70)

redMatte :: Material
redMatte = Matte (200, 10, 10)

greenMatte :: Material
greenMatte = Matte (10, 200, 10)

blueMatte :: Material
blueMatte = Matte (10, 10, 200)

vertical :: Vector
vertical = (0.0, 1.0, 0.0)
--vertical = (0.0, 0.0, 1.0)

backGroundColor :: Color
backGroundColor = white

white :: Color
white = (255, 255, 255)

blue :: Color
blue = (125, 200, 255)

mapHeight = 2000
mapWidth = 2000

apperture = 0.5

generateMap :: Image PixelRGB8
generateMap = generateImage f mapHeight mapWidth
                where   f x y = getPixelRGB8 x y

getPixelRGB8 x y = PixelRGB8 r g b
    where  (r, g, b) = tracePixel x y

tracePixel :: Int -> Int -> (Word8, Word8, Word8)
tracePixel x y = trace 8 (Ray origin direction)
    where   origin = (<+>) cameraOrigin  $ lowerLeft <+> ((percentage x mapWidth) *-> horizontal) <+> ((percentage y mapHeight) *-> vertical)
            direction = norm $ (apperture *-> lowerLeft) <+> ((apperture * (percentage x mapWidth)) *-> horizontal) <+> ((apperture * (percentage y mapHeight)) *-> vertical) <+> cameraDirection

trace :: Int -> Ray -> (Word8, Word8, Word8)
trace 0 (Ray _ direction) = skyColor direction
trace bounces ray@(Ray origin direction) = calculateColor (bounces - 1) direction $ foldl (maybeApply min) Nothing $ map (hitsBall ray) balls

hitsBall :: Ray -> Ball -> Maybe MaterialHit
hitsBall ray (Ball sphere material) = hitBall hit material
    where   hit = hitsSphere ray sphere

hitBall :: Maybe HitRecord -> Material -> Maybe MaterialHit
hitBall Nothing _ = Nothing
hitBall (Just hit) material = Just (MaterialHit hit material)

calculateColor :: Int -> Vector -> Maybe MaterialHit -> (Word8, Word8, Word8)
calculateColor bounces direction (Just (MaterialHit hitRecord material)) = calculateHitColor bounces direction hitRecord material
calculateColor _ direction Nothing = skyColor direction

calculateHitColor :: Int -> Vector -> HitRecord -> Material -> Color
calculateHitColor _ direction (HitRecord _ _ normal) (Matte color) = (0.25 + 0.75 * (abs $ (*.) normal $ norm direction)) *=> color
calculateHitColor bounces direction (HitRecord _ hitPoint normal) Reflection = trace bounces (Ray hitPoint (norm $ reflectionOnNormal direction normal))

reflectionOnNormal :: Vector -> Vector -> Vector
reflectionOnNormal v n = v <-> (((n *. v) * 2.0) *-> n)

skyColor direction = interpolateColor ((add 1.0 $ getY $ norm direction) / 2) blue white

percentage :: Int -> Int -> Double
percentage x total = (fromIntegral x) / (fromIntegral total)

createImage :: IO ()
createImage = do
    putStrLn "generating image"
    let image = generateMap
    savePngImage "ciao.png" (ImageRGB8 image)

add :: Double -> Double -> Double
add a b = a + b