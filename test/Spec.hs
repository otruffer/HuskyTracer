import Test.HUnit

import Lib
import Test.HUnit

testNorm :: Test
testNorm = TestCase $ assertEqual "Normalize a vector"
    (1 / sqrt 2, 1 / sqrt 2, 0.0) (norm (1.0, 1.0, 0.0))

testHit :: Test
testHit = TestCase $ assertEqual "you hit the sphere in the right direction"
    True (hitsSphere (Ray (0.0, 0.0, 0.0) (1.0, 0.0, 0.0)) (Sphere (2.0, 0.0, 0.0) 2))

testMiss :: Test
testMiss = TestCase $ assertEqual "you hit the sphere in the right direction"
    False (hitsSphere (Ray (0.0, 0.0, 0.0) (1.0, 0.0, 0.0)) (Sphere (1.0, 1.0, 0.0) 0.5))

testPercentage :: Test
testPercentage = TestCase $ assertEqual "percentage of 3 of 5 = 0.6"
    0.6 (percentage 3 5)

main :: IO Counts
main = runTestTT $ TestList [testNorm, testHit, testMiss, testPercentage]
