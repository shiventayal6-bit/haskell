module ListUtils
    ( catMaybes, mapMaybe, fromMaybe
    , transpose, partition, replicate
    , splitAt, take, drop, takeWhile, dropWhile
    , and, or, any, all
    , group, nub, sort, sortBy
    , exampleMaybes, exampleMatrix
    ) where

import Prelude hiding (replicate, splitAt, take, drop, takeWhile, dropWhile, and, or, any, all)
import qualified Prelude
import Data.List (group, nub, sort, sortBy)

-- | Extract Just values, discard Nothing
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs

-- | Map function returning Maybe, keep only Just results
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = catMaybes . map f

-- | Provide default for Nothing
fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x

-- | Transpose matrix (rows become columns)
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose xss = map head xss : transpose (map tail xss)

-- | Split list by predicate into (true, false)
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition p (x:xs)
  | p x       = (x:ys, ns)
  | otherwise = (ys, x:ns)
  where (ys, ns) = partition p xs

-- | Replicate value n times
replicate :: Int -> a -> [a]
replicate n x
  | n <= 0    = []
  | otherwise = x : replicate (n-1) x

-- | Split list at position
splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

-- | Take first n elements
take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs

-- | Drop first n elements
drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs

-- | Take while predicate holds
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
  | p x       = x : takeWhile p xs
  | otherwise = []

-- | Drop while predicate holds
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p xs@(x:xs')
  | p x       = dropWhile p xs'
  | otherwise = xs

-- | All elements True?
and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

-- | Any element True?
or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs

-- | Any element satisfies predicate?
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

-- | All elements satisfy predicate?
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

-- Test data
exampleMaybes :: [Maybe Int]
exampleMaybes = [Just 1, Nothing, Just 3, Nothing, Just 5]

exampleMatrix :: [[Int]]
exampleMatrix = [[1,2,3], [4,5,6], [7,8,9]]