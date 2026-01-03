module Tries where

import Data.List hiding (insert)
import Data.Bits

import Types
import HashFunctions
import Examples
import Distribution.Simple.Command (helpCommandUI)
import Distribution.Simple.Utils (xargs)
import Data.Bits (Bits(popCount))
import Types (SubNode(SubTrie))
import Text.XHtml (vspace)

--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]


countOnes :: Int -> Int
countOnes 0 = 0
countOnes n =  bitTable!!(n `mod` 16) + countOnes (n `div` 16)



countOnesFrom :: Int -> Int -> Int
countOnesFrom i n = countOnes (n .&. (bit i -1))

getIndex :: Int -> Int -> Int -> Int
getIndex n 0 b = n `mod` bit b
getIndex n i b = getIndex (n `div` bit b) (i - 1) b

-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace 0 (x:xs) c = c:xs
replace n (x:xs) c = x : replace (n-1) xs c

-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 c xs = c : xs
insertAt n c (x : xs) = x : insertAt (n-1) c xs
--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie f1 f2 (Leaf xs) = f2 xs
sumTrie f1 f2 (Node b (Term n :xs)) = f1 n + sumTrie f1 f2 (Node b xs)
sumTrie f1 f2 (Node b (SubTrie t : xs)) = sumTrie f1 f2 t + sumTrie f1 f2 (Node b xs)
sumTrie f1 f2 (Node b []) = 0

trieSize :: Trie -> Int
trieSize = sumTrie (const 1) length

binCount :: Trie -> Int
binCount = sumTrie (const 1) (const 1)

meanBinSize :: Trie -> Double
meanBinSize t = fromIntegral (trieSize t) / fromIntegral (binCount t)


member :: Int -> Hash -> Trie -> Int -> Bool
member v h (Leaf vs) b = v `elem` vs 
member v h (Node bv subs) b 
    | not (testBit bv i) = False
    | Term v' <- sub     = v == v'
    | SubTrie t' <- sub  = member v (h `div` bit b) t' b 
    where
        i = getIndex h 0 b  -- h div bit b increments value so we just keep 0
        n = countOnesFrom i bv -- value 1 are the only ones which contain further values
        sub = subs !! n   -- subs only contain non-empty buckets


--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert f d b v node
  = insert' f d b v 0 node 
  where
    insert' :: HashFun -> Int -> Int -> Int -> Int -> Trie -> Trie
    insert' f d b v _ (Leaf vs)
      | v `elem` vs = Leaf vs
      | otherwise   = Leaf (v : vs)
    insert' f 0 b v _ _
      = Leaf [v]
    insert' f d b v l node@(Node bv subs)
      | not (testBit bv i) = Node (setBit bv i) (insertAt n (Term v) subs) 
      | SubTrie t <- sub   = node'
      | otherwise          = if v == v' then node else node''
      where
        i = getIndex (f v) l b
        n = countOnesFrom i bv
        sub = subs !! n
        SubTrie t = sub
        Term v' = sub
        sub' = SubTrie (insert' f (d - 1) b v (l + 1) t)
        node' = Node bv (replace n subs sub')
        sub'' = SubTrie (insert' f (d - 2) b v (l + 1) 
                       (insert' f (d - 1) b v' (l + 1) empty))
        node'' = Node bv (replace n subs sub'')
    

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie f d b = foldr (insert f d b) empty 