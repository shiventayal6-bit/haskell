module Main where

import PerfectTrees
import ListUtils
import qualified Data.Set as Set

main :: IO ()
main = do
  putStrLn "============================================"
  putStrLn "  Perfect Trees - Complete Demo"
  putStrLn "============================================"
  putStrLn ""
  
  putStrLn "=== List Utilities Demo ==="
  putStrLn $ "catMaybes: " ++ show (catMaybes exampleMaybes)
  putStrLn $ "transpose: " ++ show (transpose exampleMatrix)
  putStrLn $ "partition even [1..10]: " ++ show (partition even [1..10])
  putStrLn $ "replicate 5 'X': " ++ replicate 5 'X'
  putStrLn $ "and [True,False]: " ++ show (and [True,False])
  putStrLn $ "or [False,True]: " ++ show (or [False,True])
  putStrLn $ "any even [1,3,5]: " ++ show (any even [1,3,5])
  putStrLn $ "all (< 10) [1..5]: " ++ show (all (< 10) [1..5])
  putStrLn ""
  
  putStrLn "=== Part I: Basic Operations ==="
  putStrLn $ "Binary tree leaves: " ++ show (leaves binaryTree)
  putStrLn $ "Leaf count: " ++ show (countLeaves binaryTree)
  putStrLn $ "Tree depth: " ++ show (treeDepth binaryTree)
  putStrLn $ "All same depth? " ++ show (allAtSameDepth binaryTree)
  putStrLn ""
  
  putStrLn "=== Part II: Perfect Trees ==="
  putStrLn $ "Is binary perfect (k=2)? " ++ show (isPerfect 2 binaryTree)
  putStrLn $ "Is ternary perfect (k=3)? " ++ show (isPerfect 3 ternaryTree)
  let built = buildPerfect 3 2 'X'
  putStrLn $ "Built 3-ary tree leaves: " ++ show (countLeaves built) ++ " (expected 9)"
  putStrLn ""
  
  putStrLn "=== Part III: Set Operations ==="
  putStrLn $ "Binary as set: " ++ show (leavesToSet binaryTree)
  putStrLn $ "Union: " ++ show (treeUnion binaryTree ternaryTree)
  putStrLn $ "Intersection: " ++ show (treeIntersection binaryTree ternaryTree)
  putStrLn $ "Is 3 in tree? " ++ show (elemTree 3 binaryTree)
  putStrLn ""
  
  putStrLn "=== Part IV: Advanced Operations ==="
  putStrLn $ "Doubled: " ++ show (leaves (mapLeaves (*2) binaryTree))
  putStrLn $ "Even leaves: " ++ show (filterLeaves even binaryTree)
  putStrLn $ "All positive? " ++ show (allLeaves (> 0) binaryTree)
  putStrLn $ "Any even? " ++ show (anyLeaf even binaryTree)
  putStrLn $ "Partition: " ++ show (partitionLeaves even binaryTree)
  putStrLn ""
  
  putStrLn "=== Part V: Transformations ==="
  putStrLn $ "Replicate tree: " ++ show (leaves (replicateTree 3 'A'))
  putStrLn $ "catMaybeLeaves: " ++ show (catMaybeLeaves maybeTree)
  case zipTreesWith (+) binaryTree binaryTree of
    Just tree -> putStrLn $ "Zip with (+): " ++ show (leaves tree)
    Nothing -> putStrLn "Zip failed"
  putStrLn ""
  
  putStrLn "============================================"
  putStrLn "Try: cabal repl, then import PerfectTrees"
  putStrLn "============================================"