module Solver where

import Data.List
import Data.Char

import Types
import WordData
import Clues
import Examples
import Distribution.Simple.Utils (xargs)
import Data.Char (toLower)
import Types (ParseTree(Synonym, Insertion))


------------------------------------------------------
-- Part I

punctuation :: String
punctuation
  = "';.,-!?"

cleanUp :: String -> String
cleanUp [] = []
cleanUp (x : xs)
    | x `elem` punctuation = cleanUp xs
    | otherwise = toLower x : cleanUp xs

split2 :: [a] -> [([a], [a])]
split2 xs = [ splitAt n xs| n <- [1..(length xs-1)] ]

split3 :: [a] -> [([a], [a], [a])]
split3 xs = [(left, middle, right) |
               (lt, rt) <- split2 xs, let right = rt, (left, middle) <- split2 lt] ++  [(left,[],right) | (left,right) <- split2 xs]
uninsert :: [a] -> [([a], [a])]
uninsert xs = [(middle,left++right) | (left,middle,right) <- split3 xs , not (null middle)]

split2M :: [a] -> [([a], [a])]
split2M xs
  = sxs ++ [(y, x) | (x, y) <- sxs]
  where
    sxs = split2 xs

split3M :: [a] -> [([a], [a], [a])]
split3M xs
  = sxs ++ [(z, y, x) | (x, y, z) <- sxs]
  where
    sxs = split3 xs


------------------------------------------------------
-- Part II

matches :: String -> ParseTree -> Bool
matches str (Synonym word) = str `elem` synonyms word
matches str (Anagram _ letters) = sort str == sort letters
matches str (Reversal _ tree) = matches (reverse str) tree
matches str (Insertion _ t1 t2) = or [matches s1 t1 && matches s2 t2 |
  (s1, s2) <- uninsert str]
matches str (Charade _ t1 t2) = or [matches s1 t1 && matches s2 t2 |
  (s1, s2) <- split2 str]

evaluate :: Parse -> Int -> [String] -- i need synonyms of unwords dedf and then match with the tree
evaluate (def, _ ,tree) n = [s | s <- synonyms (unwords def) , length s == n ,
                                 matches s tree]

------------------------------------------------------
-- Part III

-- Given...
parseWordplay :: [String] -> [ParseTree]
parseWordplay ws
  = concat [parseSynonym ws,
            parseAnagram ws,
            parseReversal ws,
            parseInsertion ws,
            parseCharade ws]

parseSynonym :: [String] -> [ParseTree]
parseSynonym xs 
  | not(null(synonyms(unwords xs))) = [Synonym (unwords xs)]
  | otherwise                       = []

parseAnagram :: [String] -> [ParseTree]
parseAnagram xs 
  |  "a" `elem` anagramIndicators = [Anagram ["a"] "b"]
  |  "b" `elem` anagramIndicators = [Anagram ["b"] "a"]
  | otherwise = []
    where [(a,b),_] = split2M xs 
-- split2M the arg, check for anag ind, if anag ind then anagram [p1] "p2"
  --  
parseReversal :: [String] -> [ParseTree]
parseReversal ws 
  = [Reversal ind t | (ind, arg) <- split2M ws,
                      unwords ind `elem` reversalIndicators,
                      t <- parseWordplay arg]

parseInsertion :: [String] -> [ParseTree]
parseInsertion s =
  [Insertion ws t t' | (arg, ws, arg') <- splits,
    unwords ws `elem` insertionIndicators,
    t <- parseWordplay arg, t' <- parseWordplay arg'] ++
  [Insertion ws t' t | (arg, ws, arg') <- splits,
    unwords ws `elem` envelopeIndicators,
    t <- parseWordplay arg, t' <- parseWordplay arg']
  where splits = split3 s 

parseCharade :: [String] -> [ParseTree]
parseCharade s = 
  [Charade ws t t' | (arg, ws, arg') <- splits,
    unwords ws `elem` beforeIndicators,
    t <- parseWordplay arg, t' <- parseWordplay arg'] ++
  [Charade ws t' t | (arg, ws, arg') <- splits,
    unwords ws `elem` afterIndicators,
    t <- parseWordplay arg, t' <- parseWordplay arg']
  where splits = split3 s

-- Given...
parseClue :: Clue -> [Parse]
parseClue clue@(s, n)
  = parseClueText (words (cleanUp s))

parseClueText :: [String] -> [Parse]
parseClueText s = [(def,link,t)| (def,link,wp) <- split3M s,
  unwords link `elem` linkWords, not $ null $ synonyms $ unwords def,
  t <- parseWordplay wp]

solve :: Clue -> [Solution]
solve clue@(_, n) = [(clue, parse, s) | parse <- parseClue clue,
  s <- evaluate parse n, not $ null s]


------------------------------------------------------
-- Some additional test functions

-- Returns the solution(s) to the first k clues.
-- The nub removes duplicate solutions arising from the
-- charade parsing rule.
solveAll :: Int -> [[String]]
solveAll k
  = map (nub . map getSol . solve . (clues !!)) [0..k-1]

getSol :: Solution -> String
getSol (_, _, sol) = sol

showAll
  = mapM_ (showSolutions . solve . (clues !!)) [0..23]

