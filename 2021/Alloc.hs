module Alloc where

import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Data.Tuple (swap)


import Types
import Examples
import Distribution.Simple.Utils (xargs)
import Data.Char (GeneralCategory(NonSpacingMark))
import Data.Foldable (Foldable(toList))
import System.Console.GetOpt (getOpt')
import Text.Read (Lexeme(Ident))
import Distribution.Types.LocalBuildInfo (unitIdTarget')

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count n [] = 0
count n (x:xs)
    | n == x = 1 + count n xs
    | otherwise = count n xs

degrees :: Eq a => Graph a -> [(a, Int)]
destruct [] = []
destruct ((n1,n2):ns) = n1:n2:destruct ns
degrees (ns,es) = [(n, count n (destruct es) ) | n <- ns]

neighbours :: Eq a => a -> Graph a -> [a]
neighbours n (ns,[]) = []
neighbours n (ns, (a,b):abs)
    | n == a = b: neighbours n ( ns, abs)
    | n == b = a: neighbours n ( ns, abs)
    | otherwise = neighbours n ( ns, abs)
removeNode :: Eq a => a -> Graph a -> Graph a
edgeLess x [] = []
edgeLess x ((a,b): abs)
    | (x == a) || (x == b) = edgeLess x abs
    | otherwise = (a,b) : edgeLess x abs
removeNode x (ns,abs) = ( ns \\ [x] , edgeLess x abs)
------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _ ([], _) = []
colourGraph c g = (minNode, nodeColour) : recursiveColouring
    where
        minNode = snd (minimum (map swap (degrees g)))
        g' = removeNode minNode g
        recursiveColouring = colourGraph c g'

        neighs = neighbours minNode g
        neighbourColours = [colour | (node, colour) <- recursiveColouring, node `elem` neighs]
        availableColours = [cs | cs <- [1..c], cs `notElem` neighbourColours]

        nodeColour = if null availableColours then 0 else head availableColours



------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap [] = [("return","return")]
buildIdMap ((v,c) : vcs)
    | c == 0 = (v,v) : buildIdMap vcs
    | otherwise = (v, 'R' : show c) : buildIdMap vcs

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments [] _ = []
buildArgAssignments (x:xs) clrs =
 Assign (lookUp x clrs) (Var x) : buildArgAssignments xs clrs


renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Const n) idmap = Const n
renameExp (Var x) idmap = Var (lookUp x idmap)
renameExp (Apply op exp exp') idmap =
    Apply op (renameExp exp idmap) (renameExp exp' idmap)


renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock [] _ = []
renameBlock (statement : rest) idmap = filter (not . isReduntant ) (renameStatement statement idmap : renameBlock rest idmap)
    where
        renameStatement :: Statement -> IdMap -> Statement
        renameStatement (Assign id exp) idmap = Assign (lookUp id idmap) (renameExp exp idmap)
        renameStatement (If exp b1 b2) idmap =
            If (renameExp exp idmap) (renameBlock b1 idmap) (renameBlock b2 idmap)
        renameStatement (While exp b) idmap = While (renameExp exp idmap) (renameBlock b idmap)
        isReduntant (Assign id (Var id')) = id == id' 
        isReduntant _ = False
renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG liveVars =
  ( nodes
  , nub [ (x, y)
        | vs <- liveVars
        , x  <- vs
        , y  <- vs
        , x < y
        ]
  )
  where
    nodes = nub (concat liveVars)

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars
  = undefined

buildCFG :: Function -> CFG
buildCFG
  = undefined