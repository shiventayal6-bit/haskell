module SOL where

import Data.List
import Data.Maybe

import Types
import TestData
import Data.Bool (Bool(True))

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp n ((a,b) : abs)
    | n == a = b
    | otherwise = lookUp n abs
-- 3 marks
vars :: Formula -> [Id]
vars f = (nub.sort) (vars' f)
    where
        vars' :: Formula -> [Id]
        vars' (Var x) = [x]
        vars' (Not f) = vars' f
        vars' (And f1 f2) = vars' f1 ++ vars' f2
        vars' (Or f1 f2) = vars' f1 ++ vars' f2
-- 1 mark
idMap :: Formula -> IdMap
idMap f = zip (vars f) [1..]

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Not (Var id))
  = Not (Var id)
toNNF (Not (Not frm))
  = toNNF frm
toNNF (Not (Or frm1 frm2))
  = And (toNNF (Not frm1)) (toNNF (Not frm2))
toNNF (Not (And frm1 frm2))
  = Or (toNNF (Not frm1)) (toNNF (Not frm2))
toNNF (Var id)
  = Var id
toNNF (Or frm1 frm2)
  = Or (toNNF frm1) (toNNF frm2)
toNNF (And frm1 frm2)
  = And (toNNF frm1) (toNNF frm2)

-- 3 marks
toCNF :: Formula -> CNF
toCNF formula
  = toCNF' (toNNF formula)
  where
    toCNF' (Var id)
      = Var id
    toCNF' (Not (Var id))
      = Not (Var id)
    toCNF' (And frm1 frm2)
      = And (toCNF' frm1) (toCNF' frm2)
    toCNF' (Or frm1 frm2)
      = distribute frm1 frm2
-- 4 marks
flatten :: CNF -> CNFRep
flatten f = flatten' f (idMap f)
    where
        flatten' :: CNF -> IdMap -> CNFRep
        flatten' (Var id) m = [[lookUp id m]]
        flatten' (Not (Var id)) m = [[- (lookUp id m)]]
        flatten' (And f1 f2) m = flatten' f1 m ++ flatten' f2 m
        flatten' (Or f1 f2) m = [concat (flatten' f1 m) ++ concat (flatten' f2 m)]
--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits cnf = propUnits' cnf []
    where
        propUnits' cnf units
            | null unitLits = (cnf, units)
            | otherwise     = propUnits' cnf' (u : units)
            where
                unitLits = [u | [u] <- cnf]  -- find ALL units at once
                u = head unitLits             -- pick one
                cnf' = map (filter (/= -u)) (filter (u `notElem`) cnf)  -- simplify ALL clauses



-- 4 marks
dp :: CNFRep -> [[Int]]
dp cnfRep
  | null nextRep
    = [sols]
  | [] `elem` nextRep
    = []
  | otherwise
    = map (sols++) (dp ([literal] : nextRep) ++ dp ([-literal] : nextRep))
  where
    (nextRep, sols) = propUnits cnfRep
    literal = head (head nextRep)

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat frm
  = allResults'
  where
    cnfRep = flatten (toCNF (toNNF frm))
    result = dp cnfRep
    idMaps = idMap frm
    maxEl = length idMaps
    allResults = concatMap (makeWholeSorted 1 maxEl) result
    allResults' = map (bindResult idMaps) allResults

-- This function takes as it's 3rd parameter a solution and
-- adds all possible missing numbers to it
makeWholeSorted :: Int -> Int -> [Int] -> [[Int]]
makeWholeSorted n maxEl list
  | n > maxEl
    = [[]]
  | n `elem` list
    = map (n :) (makeWholeSorted (n+1) maxEl list)
  | (-n) `elem` list
    = map ((-n) :) (makeWholeSorted (n+1) maxEl list)
  | otherwise
    = makeWholeSorted n maxEl (n : list) ++
      makeWholeSorted n maxEl ((-n) : list)

-- This is the inverse of the lookUp function
inverseLookUp :: Eq b => b -> [(a, b)] -> a
inverseLookUp el list
  = lookUp el (map (\(x,y) -> (y,x)) list)

-- This creates a result from an input like
-- [("var", 1)] [-1] to [("var", False)]
bindResult :: IdMap -> [Int] -> [(Id, Bool)]
bindResult idMaps = map makePair
  where
    makePair n
      | n < 0
        = (inverseLookUp (-n) idMaps, False)
      | otherwise
        = (inverseLookUp n idMaps, True)

f1 :: Formula
f1 = Not (Not (Var "a"))

f2 :: Formula
f2 = And (Or (Not (Var "c")) (Not (Var "g"))) (Or (Var "c") (Not (Var "e")))

f3 :: Formula
f3 = And (Not (Or (Var "c") (Var "g"))) (Or (Var "c") (Not (Not (Var "e"))))

