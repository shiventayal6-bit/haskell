module PerfectTrees
    ( Tree(..), BranchingFactor
    , countLeaves, treeDepth, leaves, allAtSameDepth
    , isPerfect, buildPerfect, buildPerfectFrom
    , leavesToSet, elemTree, treeUnion, treeIntersection, uniqueLeaves
    , mapLeaves, filterLeaves, allLeaves, anyLeaf, partitionLeaves
    , replicateTree, transposeForest, catMaybeLeaves, zipTrees, zipTreesWith
    , binaryTree, ternaryTree, imperfectTree, pentaryTree, maybeTree
    ) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified ListUtils as LU

type BranchingFactor = Int

data Tree a = Leaf a | Node [Tree a]
    deriving (Eq, Show)

-- Example trees
binaryTree :: Tree Int
binaryTree = Node [Node [Leaf 1, Leaf 2], Node [Leaf 3, Leaf 4]]

ternaryTree :: Tree Int
ternaryTree = Node [Leaf 1, Leaf 2, Leaf 3]

imperfectTree :: Tree Int
imperfectTree = Node [Leaf 1, Node [Leaf 2, Leaf 3]]

pentaryTree :: Tree Int
pentaryTree = Node [Leaf 1, Leaf 2, Leaf 3, Leaf 4, Leaf 5]

maybeTree :: Tree (Maybe Int)
maybeTree = Node [Leaf (Just 1), Leaf Nothing, Leaf (Just 3), Leaf Nothing]

-- Part I: Basic Operations
countLeaves :: Tree a -> Int
countLeaves (Leaf _) = 1
countLeaves (Node children) = sum (map countLeaves children)

treeDepth :: Tree a -> Int
treeDepth (Leaf _) = 0
treeDepth (Node children) = 1 + maximum (map treeDepth children)

leaves :: Tree a -> [a]
leaves (Leaf x) = [x]
leaves (Node children) = concatMap leaves children

allAtSameDepth :: Tree a -> Bool
allAtSameDepth tree = checkDepth tree /= Nothing
  where
    checkDepth (Leaf _) = Just 0
    checkDepth (Node []) = Nothing
    checkDepth (Node children) = 
      let depths = map checkDepth children
      in if LU.all (== head depths) depths && head depths /= Nothing
         then fmap (+1) (head depths)
         else Nothing

-- Part II: Perfect Tree Operations
isPerfect :: BranchingFactor -> Tree a -> Bool
isPerfect k tree 
  | k < 2 = False
  | otherwise = allAtSameDepth tree && allHaveKChildren k tree
  where
    allHaveKChildren _ (Leaf _) = True
    allHaveKChildren k (Node children) = 
      length children == k && LU.all (allHaveKChildren k) children

buildPerfect :: BranchingFactor -> Int -> a -> Tree a
buildPerfect k 0 x = Leaf x
buildPerfect k d x = Node (LU.replicate k (buildPerfect k (d-1) x))

buildPerfectFrom :: BranchingFactor -> Int -> [a] -> Maybe (Tree a)
buildPerfectFrom k d xs
  | length xs /= k ^ d = Nothing
  | otherwise = fst <$> build d xs
  where
    build 0 (x:xs) = Just (Leaf x, xs)
    build 0 [] = Nothing
    build depth xs = buildChildren k xs
      where
        buildChildren 0 xs = Just (Node [], xs)
        buildChildren n xs = do
          (child, xs') <- build (depth - 1) xs
          (Node children, xs'') <- buildChildren (n - 1) xs'
          return (Node (child : children), xs'')

-- Part III: Set Operations
leavesToSet :: Ord a => Tree a -> Set a
leavesToSet = Set.fromList . leaves

elemTree :: Ord a => a -> Tree a -> Bool
elemTree x tree = Set.member x (leavesToSet tree)

treeUnion :: Ord a => Tree a -> Tree a -> Set a
treeUnion t1 t2 = Set.union (leavesToSet t1) (leavesToSet t2)

treeIntersection :: Ord a => Tree a -> Tree a -> Set a
treeIntersection t1 t2 = Set.intersection (leavesToSet t1) (leavesToSet t2)

uniqueLeaves :: Ord a => Tree a -> [a]
uniqueLeaves = Set.toList . leavesToSet

-- Part IV: Advanced Operations
mapLeaves :: (a -> b) -> Tree a -> Tree b
mapLeaves f (Leaf x) = Leaf (f x)
mapLeaves f (Node children) = Node (map (mapLeaves f) children)

filterLeaves :: (a -> Bool) -> Tree a -> [a]
filterLeaves p = filter p . leaves

allLeaves :: (a -> Bool) -> Tree a -> Bool
allLeaves p = LU.all p . leaves

anyLeaf :: (a -> Bool) -> Tree a -> Bool
anyLeaf p = LU.any p . leaves

partitionLeaves :: (a -> Bool) -> Tree a -> ([a], [a])
partitionLeaves p = LU.partition p . leaves

-- Part V: Tree Transformations
replicateTree :: Int -> a -> Tree a
replicateTree n x = Node (LU.replicate n (Leaf x))

transposeForest :: [Tree a] -> [Tree a]
transposeForest trees = map (Node . map Leaf) (LU.transpose (map leaves trees))

catMaybeLeaves :: Tree (Maybe a) -> [a]
catMaybeLeaves = LU.catMaybes . leaves

zipTrees :: Tree a -> Tree b -> Maybe (Tree (a, b))
zipTrees (Leaf x) (Leaf y) = Just (Leaf (x, y))
zipTrees (Node xs) (Node ys)
  | length xs == length ys = fmap Node (sequence (zipWith zipTrees xs ys))
  | otherwise = Nothing
zipTrees _ _ = Nothing

zipTreesWith :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTreesWith f t1 t2 = fmap (mapLeaves (\(x,y) -> f x y)) (zipTrees t1 t2)