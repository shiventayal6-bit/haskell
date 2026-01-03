{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Use foldr" #-}
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map(Map)
import Data.Map qualified as Map
import Data.Char
import Control.Monad (guard)





myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n [] = []
myTake n (x:xs) = x : myTake (n-1) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = reverse xs ++ [x]

rev :: [a] -> [a]
rev xs = rev' xs []
    where
        rev' :: [a] -> [a] -> [a]
        rev' [] ys = ys
        rev' (x:xs) ys = rev' xs (x:ys)

any1 :: ( a -> Bool) -> [a] -> Bool
any1 p xs = (or. map p) xs

reverse1 :: [a] -> [a]
reverse1 xs = reverse1' [] xs
    where
        reverse1' :: [a] -> [a] -> [a]
        reverse1' y [] = y
        reverse1' y (x:xs) = reverse1' (x:y) xs

data Tree a where
    Tip :: Tree a
    Node :: Tree a -> a -> Tree a -> Tree a
    deriving (Show, Eq)
insertTree :: Ord a => a -> Tree a -> Tree a
insertTree x Tip = Node Tip x Tip
insertTree x (Node lt y rt)
    | x <= y = Node (insertTree x lt) y rt
    | otherwise = Node lt y (insertTree x rt)

grow :: Ord a => [a] -> Tree a
grow = foldr insertTree Tip

nubOrd :: forall a. Ord a => [a] -> [a]
nubOrd xs = nub' Set.empty xs
    where nub' :: Set a -> [a] -> [a]
          nub' _ [] = []
          nub' seen (x:xs)
            | Set.member x seen = nub' seen xs
            | otherwise         = x : nub' ( Set.insert x seen) xs

nubOrdBad :: Ord a => [a] -> [a]
nubOrdBad = Set.toList . Set.fromList

ordChar :: Map Char Int
ordChar = Map.fromList [(c,ord c) | c <- ['\0'..'\255']]

groupBy :: Ord k => (a -> k) -> [a] -> Map k [a]
groupBy f [] = Map.empty
groupBy f (x:xs) = Map.insertWith (++) (f x) [x] m
       where m = groupBy f xs

--map :: (a -> b) -> [a] -> [b]
--map f [] = []
--map f (x:xs) = (f x) : map f xs 

data Bush a = Leaf a | Fork (Bush a) (Bush a) deriving (Show,Functor)
data RoseBush a = Flower a | Vine [RoseBush a] deriving (Show,Functor)

t1 :: Tree Int
t1 = Node (Node Tip 4 Tip) 6 Tip

b1 :: Bush Char
b1 = Fork (Leaf 'a') (Fork (Leaf 'c') (Leaf 'b'))

rb1 :: RoseBush Int
rb1 = Vine [Vine [Flower 1],Flower 2, Vine [Flower 3,Flower 4]]

mapBush :: (a -> b) -> Bush a -> Bush b
mapBush f (Leaf x) = Leaf (f x)
mapBush f (Fork lb rb) = Fork (mapBush f lb) (mapBush f rb)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Tip = Tip
mapTree f (Node lt x rt) = Node (mapTree f lt) (f x) (mapTree f rt)

mapRoseBush :: (a -> b) -> RoseBush a -> RoseBush b
mapRoseBush f (Flower a) = Flower (f a)
mapRoseBush f (Vine rbs) = Vine (map (mapRoseBush f) rbs)
-- (mapRose f) acts as a function with type RoseBush a to RoseBush b

--all maps are already gneralised in terms of fmap just derive functor and use fmap

safeDiv :: Integral a => a -> a -> Maybe a
safeDiv m 0 = Nothing
safeDiv m n = Just (m `div` n)

square :: Num a => a -> a
square x = x* x

safeDivThenSqaure :: Integral a => a -> a -> Maybe a
safeDivThenSqaure m n = fmap square (safeDiv m n)
-- fmap on a maybe 

andMaybe :: Maybe Bool -> Maybe Bool -> Maybe Bool
andMaybe Nothing _ = Nothing
andMaybe (Just x) my = fmap (x && ) my

lessMaybe :: Ord a => Maybe a -> Maybe a -> Maybe Bool
lessMaybe Nothing _ = Nothing
lessMaybe (Just x) my = fmap (x < ) my

consMaybe :: Maybe a -> Maybe [a] -> Maybe [a]
consMaybe Nothing _ = Nothing
consMaybe (Just x) mxs = fmap (x :) mxs

productWithMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
productWithMaybe f Nothing _ = Nothing
productWithMaybe f (Just x) my = fmap (f x) my

productWithList :: (a -> b -> c) -> [a] -> [b] -> [c]
productWithList f xs ys = [f x y | x <- xs, y <- ys]

--productwith is liftA2 , so technically fmap is liftA

{-liftA :: Functor t => (a -> b) -> t a -> t b
liftA2 :: ? t => (a -> b -> c) -> t a -> t b -> t c
liftA3 :: ? t => (a -> b -> c -> d) -> t a -> t b -> t c -> t d 
-}

{-prod :: [a] -> [b] -> [(a,b)]
prod xs ys = [(x,y)| x <- xs, y <- ys]
-}

prod :: [a] -> [b] -> [(a,b)]
prod xs ys = do { x <- xs ;
                  y <- ys ;
                  return (x,y)}

{- 
pyths :: [(Integer,Integer,Integer)]
pyths = [(a,b,h)| h <- [1..75],
                  b <- [1..h],
                  a <- [1..b]
                  ,a^2 + b^2 == h^2]
-}

pyths :: [(Integer,Integer,Integer)]
pyths = do h <- [1..75]
           b <- [1..h]
           a <- [1..b]
           guard (a^2 + b^2 == h^2)
           return (a,b,h)

main :: IO()
main =
    do print "What is your name?"
       input <- getLine
       print ("Hello " ++ input)
       return ()
half :: Int -> Maybe Int
half x
    | even x = Just (x `div` 2)
    | otherwise      = Nothing

testMonad :: Maybe Int
testMonad = Just 20 >>= half >>= half >>= half

testMonad2 :: Maybe Int
testMonad2 = Just 20 >>= half >>= half

-- monads useful when basically function doesnt guarantee 
-- an output like half function above or safeDiv :: maybe int ->
    -- maybe int -> ..

depunctuate :: [Char] -> [Char]
depunctuate = filter (`notElem` ['.', ',', ':'] )

makeString :: [Int] -> [Char]
makeString = map chr

enpower :: [Int] -> Int
enpower (x:xs) = foldr (^) x xs

revAll :: [[a]] -> [a]
revAll = concatMap rev

{- rev :: [a] -> [a]
rev xs = rev' xs []
where rev' [] ys = ys
rev' (x : xs) ys = rev' xs (x : ys)
-}
rev' :: [a] -> [a]
rev' = foldl f []
    where f ys x = x: ys

dezip :: [(a,b)] -> ([a],[b])
dezip = foldr (\(x,y) (xs, ys) -> (x:xs,y:ys)) ([],[])

allSame :: [Int] -> Bool
allSame [] = True
allSame xs = and ( zipWith (==) xs (tail xs))

lolu :: [Int]
lolu = scanl (*) 1 [1..]

approx :: Float
approx = sum (map (recip.fromIntegral ) (take 5 lolu))

mystery :: [Integer]
mystery = 1 : scanl (+) 1 mystery

squash :: (a -> a -> b) -> [a] -> [b]
squash f xs = zipWith f xs (tail xs)

converge :: (a -> a -> Bool) -> [a] -> a
converge f [x] = x
converge f (x1 : x2 : xs)
    | f x1 x2 = x1
    | otherwise = converge f (x2 : xs)

polu :: Float
polu = converge lim (sums (map (recip.fromIntegral) lolu))
    where lim x y = abs (x - y) < 0.00001
          sums = scanl (+) 0

limit :: (a -> a -> Bool) -> [a] -> [a]
limit f (x1 : x2 : xs)
    | f x1 x2 = [x1,x2]
    | otherwise = x1 : limit f (x2 : xs)
limit _ xs = xs

data Shape = Triangle Float Float Float
           | Square Float
           | Circle Float
           | Polygon [(Float,Float)]

area :: Shape -> Float
area (Triangle a b c) = sqrt (s*(s - a)* (s-b) *(s-c))
    where s = (a+b+c)/2

area (Polygon (v1 : v2 : v3 : vs)) = area (Triangle a b c) + area (Polygon (v1 : v3 : vs))
    where
        a = dist v1 v2
        b = dist v2 v3
        c = dist v3 v1
        dist :: Floating a => (a, a) -> (a, a) -> a
        dist (x, y) (x', y') = sqrt ((x - x') ^ 2 + (y - y') ^ 2)
area (Polygon _) = 0

data Date = Data Int Int Int

age :: Date -> Date -> Int
age (Data d m y) (Data d' m' y')
    | (m, d) <= (m', d') = y' - y
    | otherwise = y' - y - 1

data Tree' = Leaf' | Node' Tree' Tree' deriving (Eq, Show)

makeTrees :: Int -> [Tree']
makeTrees 0 = [Leaf']
makeTrees n = [Node' left right | lt <- [0..(n-1)], left <- makeTrees lt, right <- makeTrees (n - lt -1)]

data Tree'' a where
    Leaf'' :: a -> Tree'' a
    Node'' :: Tree'' a -> Tree'' a -> Tree'' a
    deriving (Show, Eq)
build :: [a] -> Tree'' a
build [x] = Leaf'' x
build xs = Node'' (build left) (build right)
    where
        (left, right) = splitAt (length xs `div` 2) xs

ends :: Tree'' a -> [a]
ends (Leaf'' n) = [n]
ends (Node'' lt rt) = ends lt ++ ends rt

swap :: Tree'' a -> Tree'' a
swap (Leaf'' n)= Leaf'' n
swap (Node'' lt rt) = Node'' (swap rt) (swap lt)

sequence' :: Applicative f => [f a] -> f [a]
sequence' [] = pure []
sequence' (mx:mxs) = (:) <$> mx <*> sequence' mxs

traverse' :: Applicative f => (a1 -> f a2) -> [a1] -> f [a2]
traverse' f = sequence' . map f

replicateM :: Applicative f => Int -> f a -> f [a]
replicateM n mx = sequence' (replicate n mx)

powerSet :: [a] -> [[a]]
powerSet xs = map (\bs -> [x | (b, x) <- zip bs xs, b == 1]) (bitstrings (length xs))

bitstrings :: Int -> [[Int]]
bitstrings n = replicateM n bits
bits :: [Int]
bits = [0,1]

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb mx = do
    b <- mb
    if b then mx else pure ()

whileM :: Monad m => m Bool -> m ()
whileM mb = whenM mb (whileM mb)

farewell :: IO ()
farewell = whileM $ do
    putStrLn "Will you miss us?"
    (/= "yes") <$> getLine