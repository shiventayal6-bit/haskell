module Main where

import Test.HUnit
import PerfectTrees
import ListUtils
import qualified Data.Set as Set
import System.Exit

-- List Utils Tests
testCatMaybes = TestList
  [ [1,3,5] ~=? catMaybes [Just 1, Nothing, Just 3, Nothing, Just 5]
  , [] ~=? catMaybes [Nothing, Nothing :: Maybe Int]
  , ["a","b"] ~=? catMaybes [Just "a", Just "b"]
  ]

testTranspose = TestList
  [ [[1,4],[2,5],[3,6]] ~=? transpose [[1,2,3],[4,5,6]]
  , ["adg","beh","cfi"] ~=? transpose ["abc","def","ghi"]
  , [[1,2,3]] ~=? transpose [[1],[2],[3]]
  ]

testPartition = TestList
  [ ([2,4,6,8,10],[1,3,5,7,9]) ~=? partition even [1..10]
  , ([6,7,8,9,10],[1,2,3,4,5]) ~=? partition (> 5) [1..10]
  , ("aaa","bnn") ~=? partition (== 'a') "banana"
  ]

testReplicate = TestList
  [ "aaaaa" ~=? replicate 5 'a'
  , [[1],[1],[1]] ~=? replicate 3 [1]
  , [] ~=? replicate 0 "x"
  , [] ~=? replicate (-5) 'y'
  ]

testBoolOps = TestList
  [ True ~=? and [True,True,True]
  , False ~=? and [True,False]
  , True ~=? and []
  , True ~=? or [False,True]
  , False ~=? or [False,False]
  , False ~=? or []
  , True ~=? any even [1,2,3]
  , False ~=? any even [1,3,5]
  , True ~=? all even [2,4,6]
  , False ~=? all even [2,3,4]
  ]

testTakeDrop = TestList
  [ [1,2,3] ~=? take 3 [1..10]
  , [1,2] ~=? take 5 [1,2]
  , [4,5,6] ~=? drop 3 [1..6]
  , [] ~=? drop 10 [1,2]
  , ([1,2,3],[4,5,6]) ~=? splitAt 3 [1..6]
  , [1,2,3,4] ~=? takeWhile (< 5) [1..10]
  , [5,6,7] ~=? dropWhile (< 5) [1..7]
  ]

-- Tree Tests
testCountLeaves = TestList
  [ 1 ~=? countLeaves (Leaf 42)
  , 4 ~=? countLeaves binaryTree
  , 3 ~=? countLeaves ternaryTree
  , 8 ~=? countLeaves (buildPerfect 2 3 1)
  , 27 ~=? countLeaves (buildPerfect 3 3 0)
  ]

testTreeDepth = TestList
  [ 0 ~=? treeDepth (Leaf 5)
  , 2 ~=? treeDepth binaryTree
  , 1 ~=? treeDepth ternaryTree
  , 5 ~=? treeDepth (buildPerfect 2 5 'x')
  ]

testLeaves = TestList
  [ ['x'] ~=? leaves (Leaf 'x')
  , [1,2,3,4] ~=? leaves binaryTree
  , [1,2,3] ~=? leaves ternaryTree
  ]

testAllAtSameDepth = TestList
  [ True ~=? allAtSameDepth (Leaf 1)
  , True ~=? allAtSameDepth binaryTree
  , False ~=? allAtSameDepth imperfectTree
  ]

testIsPerfect = TestList
  [ True ~=? isPerfect 2 (Leaf 1)
  , True ~=? isPerfect 2 binaryTree
  , False ~=? isPerfect 3 binaryTree
  , True ~=? isPerfect 3 ternaryTree
  , False ~=? isPerfect 2 imperfectTree
  , True ~=? isPerfect 5 pentaryTree
  , False ~=? isPerfect 1 (Leaf 1)
  ]

testBuildPerfect = TestList
  [ Leaf 'x' ~=? buildPerfect 2 0 'x'
  , 8 ~=? countLeaves (buildPerfect 2 3 1)
  , 25 ~=? countLeaves (buildPerfect 5 2 1)
  , 27 ~=? countLeaves (buildPerfect 3 3 0)
  ]

testBuildPerfectFrom = TestList
  [ Just (Node [Node [Leaf 1, Leaf 2], Node [Leaf 3, Leaf 4]]) 
      ~=? buildPerfectFrom 2 2 [1,2,3,4]
  , Nothing ~=? buildPerfectFrom 2 2 [1,2,3]
  , Nothing ~=? buildPerfectFrom 2 2 [1,2,3,4,5]
  , Just (Leaf 'x') ~=? buildPerfectFrom 2 0 ['x']
  ]

testSetOps = TestList
  [ Set.fromList [1,2,3,4] ~=? leavesToSet binaryTree
  , Set.fromList [1,2,3] ~=? leavesToSet (Node [Leaf 1, Leaf 2, Leaf 1, Leaf 3])
  , True ~=? elemTree 3 binaryTree
  , False ~=? elemTree 10 binaryTree
  , Set.fromList [1,2,3,4] ~=? treeUnion binaryTree ternaryTree
  , Set.fromList [1,2,3] ~=? treeIntersection binaryTree ternaryTree
  , [1,2,3,4] ~=? uniqueLeaves binaryTree
  ]

testAdvanced = TestList
  [ Node [Node [Leaf 2,Leaf 4],Node [Leaf 6,Leaf 8]] ~=? mapLeaves (*2) binaryTree
  , [2,4] ~=? filterLeaves even binaryTree
  , True ~=? allLeaves (> 0) binaryTree
  , False ~=? allLeaves even binaryTree
  , True ~=? anyLeaf even binaryTree
  , False ~=? anyLeaf (> 10) binaryTree
  , ([2,4],[1,3]) ~=? partitionLeaves even binaryTree
  ]

testTransformations = TestList
  [ ['A','A','A'] ~=? leaves (replicateTree 3 'A')
  , [1,3] ~=? catMaybeLeaves maybeTree
  ]

-- All tests
allTests = TestList
  [ TestLabel "catMaybes" testCatMaybes
  , TestLabel "transpose" testTranspose
  , TestLabel "partition" testPartition
  , TestLabel "replicate" testReplicate
  , TestLabel "boolOps" testBoolOps
  , TestLabel "takeDrop" testTakeDrop
  , TestLabel "countLeaves" testCountLeaves
  , TestLabel "treeDepth" testTreeDepth
  , TestLabel "leaves" testLeaves
  , TestLabel "allAtSameDepth" testAllAtSameDepth
  , TestLabel "isPerfect" testIsPerfect
  , TestLabel "buildPerfect" testBuildPerfect
  , TestLabel "buildPerfectFrom" testBuildPerfectFrom
  , TestLabel "setOps" testSetOps
  , TestLabel "advanced" testAdvanced
  , TestLabel "transformations" testTransformations
  ]

main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn "  Running Test Suite"
  putStrLn "=========================================="
  counts <- runTestTT allTests
  putStrLn ""
  if errors counts + failures counts == 0
    then do
      putStrLn "✓ All tests passed!"
      exitSuccess
    else do
      putStrLn "✗ Some tests failed"
      exitFailure