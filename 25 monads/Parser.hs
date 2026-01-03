module Parser where

import Types
import Lexer
import Examples

import Data.Maybe
import Types (Error(Unexpected, IntNotFound, ExprNotFound))

------------------------------------------------------------------------------
-- Given...

showToken :: Token -> String
showToken (Ident v) = v
showToken (Nat v) = show v
showToken WhileTok = "while"
showToken t = [head [c | (c, t') <- tokenTable, t == t']]

printParse :: String -> IO ()
printParse input = either printError printOK (parse input)
  where
    printOK prog = putStrLn "Parse successful..." >> print prog
    printError err = putStr "Parse error: " >> printError' err
    printError'' t s = putStrLn (s ++ " expected, but " ++
                                 maybe "nothing" showToken t ++ " found")
    printError' (BadChar c) = do putStr "Unrecognised character: "
                                 putStrLn [c]
    printError' (Unexpected t t') = printError'' t (showToken t')
    printError' (StmtNotFound t) = printError'' t "Statement"
    printError' (ExprNotFound t) = printError'' t "Expression"
    printError' (IntNotFound t) = printError'' t "Integer literal"
    printError' (UnparsedInput toks) = putStrLn ("Unparsed input: " ++
                                                 unwords (map showToken toks))

------------------------------------------------------------------------------

-- Given...
mHead :: [a] -> Maybe a
mHead (x : _) = Just x
mHead _ = Nothing

checkTok :: Token -> [Token] -> Either Error [Token]
checkTok t ts
    | Just t == mHead ts = Right (tail ts)
    | otherwise                = Left (Unexpected (mHead ts) t)

parseAtom :: Parser Expr
parseAtom (Ident x : rest)       = Right (rest, Var x)
parseAtom (Nat n : rest)         = Right (rest, Val n)
parseAtom (Minus : Nat n : rest) = Right (rest, Val (negate n))
parseAtom (Minus : rest)         = Left (IntNotFound (mHead rest))
parseAtom toks                      = Left (ExprNotFound (mHead toks))

parseTerm :: Parser Expr


parseExpr :: Parser Expr
parseExpr = parseTerm

parseStmt :: Parser Stmt
parseStmt  (Ident v : Eq : rest) = do
        (remaining, expr) <- parseExpr rest
        return (remaining, Asgn v expr)
    
parseStmt toks = Left (StmtNotFound (mHead toks))

parseBlock :: Parser Block
parseBlock = undefined

parse :: String -> Either Error Program
parse input = undefined