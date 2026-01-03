module SC where

import Data.List
import Data.Maybe

import Types
import Examples

---------------------------------------------------------

prims :: [Id]
prims
  = ["+", "-", "*", "<=", "ite"]

lookUp :: Id -> [(Id, a)] -> a
lookUp v env
  = fromMaybe (error ("lookUp failed with search key " ++ v))
              (lookup v env)

---------------------------------------------------------
-- Part I

isFun :: Exp -> Bool
isFun (Fun _ _) = True
isFun _  = False

splitDefs :: [Binding] -> ([Binding], [Binding])
splitDefs [] = ([],[])
splitDefs ((id,exp): rest)
    | isFun exp = ((id,exp):fxns, vars)
    | otherwise = (fxns, (id,exp): vars)
        where
            (fxns,vars) = splitDefs rest



topLevelFunctions :: Exp -> Int
topLevelFunctions (Let bs exps) = length (fst (splitDefs bs))
topLevelFunctions _ = 0

---------------------------------------------------------
-- Part II

unionAll :: Eq a => [[a]] -> [a]
unionAll = foldl' union []

freeVars :: Exp -> [Id]
freeVars (Const _) = []
freeVars (Var x)
    | x `notElem` prims = [x]
    | otherwise         = []
freeVars (Fun ids exp) = freeVars exp \\ ids
freeVars (App exp exps) = unionAll (freeVars exp : map freeVars exps)
freeVars (Let bs e) = allFreeVars \\ boundNames
    where
        allFreeVars = unionAll (freeVars e : map (freeVars . snd) bs)
        boundNames = map fst bs
---------------------------------------------------------
-- Part III

-- Given...
lambdaLift :: Exp -> Exp
lambdaLift e
  = lift (modifyFunctions (buildFVMap e) e)

buildFVMap :: Exp -> [(Id, [Id])]
buildFVMap
  = sort . buildFVMap'
  where
    buildFVMap' :: Exp -> [(Id, [Id])]
    buildFVMap' (Fun _ e)
      = buildFVMap' e
    buildFVMap' (App e as)
      = concatMap buildFVMap' (e:as)
    buildFVMap' (Let bs e)
      = concat (vss:map buildFVMap' (e:map snd bs))
      where
        (fbs, _) = splitDefs bs
        ns       = map fst fbs
        vs       = sort . unionAll $ map ((\\ ns) . freeVars . snd) fbs
        vss      = [(id, vs) | (id, _) <- fbs]
    buildFVMap' _
      = []

modifyFunctions :: [(Id, [Id])] -> Exp -> Exp
-- Pre: The mapping table contains a binding for every function
-- named in the expression.
modifyFunctions vss e@(Var id)
  = case lookup id vss of
      Just vs@(_:_) -> App (Var ('$':id)) $ map Var vs
      Just []       -> Var ('$':id)
      Nothing       -> e
modifyFunctions vss (App e as)
  = App (modifyFunctions vss e) $ modifyFunctions vss <$> as
modifyFunctions vss (Let bs e)
  = Let (map rewriteFunDef bs) $ modifyFunctions vss e
  where
    rewriteFunDef :: Binding -> Binding
    -- As "id" is a function in the first rule, lookUp is safe
    rewriteFunDef (id, Fun as e)
      = ('$':id, Fun (lookUp id vss ++ as) $ modifyFunctions vss e)
    rewriteFunDef (id, e)
      = (id, modifyFunctions vss e)
modifyFunctions _ e
  = e


-- The default definition here is id.
-- If you implement the above two functions but not this one
-- then lambdaLift above will remove all the free variables
-- in functions; it just won't do any lifting.
lift :: Exp -> Exp
lift e
  | null scs  = e'
  | otherwise = Let scs e'
  where
    (e', scs) = lift' e

-- You may wish to use this...
lift' :: Exp -> (Exp, [Supercombinator])
lift' (App e as)
  = (App e' as', concat (escs:ascs))
  where
    (e', escs)  = lift' e
    (as', ascs) = unzip $ map lift' as
lift' (Fun as e)
  = (Fun as e', escs)
  where
    (e', escs) = lift' e
lift' (Let bs e)
  | null vbs  = (e', scs)
  | otherwise = (Let vbs e', scs)
  where
    (e', escs)  = lift' e
    -- Here ns is a list of defined names, ess is a list of new definition
    -- bodies and scss is a list of lists of supercombinators produced by
    -- recursive "lift'" calls
    (ns, ls)    = unzip $ map (lift' <$>) bs
    (ess, scss) = unzip ls
    bs'         = zip ns ess
    (fbs, vbs)  = splitDefs bs'
    scs         = concat (fbs:escs:scss)
lift' e
  = (e, [])