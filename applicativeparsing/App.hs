module ApplicativeParsing where

data Command = F | L | R | B [Command] deriving (Eq, Show)

cmdMap :: [(Char,Command)]
cmdMap = [('N', F), ('M', F), ('+', L), ('-', R)]

newtype Parser a = Parser {parse :: String -> [(a, String)]} deriving Functor

satisfy :: (Char -> Bool) -> Parser Char 
satisfy f = Parser $ eat 
    where eat :: String -> [(Char,String)]
          eat (c : cs) | f c = [(c,cs)]
          eat _              = []

instance Applicative Parser where
    pure :: a -> Parser a 
    pure x = Parser $ \cs -> [(x,cs)]

    liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
    liftA2 f p q = Parser $ \cs -> 
        [(f x y , cs'') | (x, cs') <- parse p cs
                        , (y, cs'') <- parse q cs']

