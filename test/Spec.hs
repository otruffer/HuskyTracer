import Test.HUnit

import Ray
import Linear
import Test.HUnit

testNorm :: Test
testNorm = TestCase $ assertEqual "Normalize a vector"
    (1 / sqrt 2, 1 / sqrt 2, 0.0) (norm (1.0, 1.0, 0.0))

testHit :: Test
testHit = TestCase $ assertEqual "you hit the sphere in the right direction"
    (Just (HitRecord 0.5 (0.5, 0.0, 0.0) (-1.0, 0.0, 0.0))) (hitsSphere (Ray (0.0, 0.0, 0.0) (1.0, 0.0, 0.0)) (Sphere (1.0, 0.0, 0.0) 0.5))

testMiss :: Test
testMiss = TestCase $ assertEqual "you miss the sphere in the right direction"
    Nothing (hitsSphere (Ray (0.0, 0.0, 0.0) (1.0, 0.0, 0.0)) (Sphere (1.0, 1.0, 0.0) 0.5))

testPercentage :: Test
testPercentage = TestCase $ assertEqual "percentage of 3 of 5 = 0.6"
    0.6 (percentage 3 5)

testWordScalarMultiply :: Test
testWordScalarMultiply = TestCase $ assertEqual "multply color"
    (100, 100, 100) (0.5 *=> (200, 200, 200))


main :: IO Counts
main = runTestTT $ TestList [testNorm, testHit, testMiss, testPercentage, testWordScalarMultiply]
