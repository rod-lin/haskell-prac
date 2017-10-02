{-# LANGUAGE DeriveFunctor #-}

module OperatorParser
    ( OpTree(..)
    , Associativity(..)
    , op
    , foldTree
    , parseOperators
    , module Text.ParserCombinators.ReadP
    ) 
where

import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)
import Data.Char

-- | Type for operator parse results. 'a' is the type of the operator, 'b'
-- | of the terms.
data OpTree a b = Op (OpTree a b) a (OpTree a b) 
                | Term b 
                deriving (Show, Eq, Functor)

-- | Type for specifying the assocativity of operators: left, right, or none.
-- NOTE: non-associative means the return type is different from the term TryParser
-- so it cannot be nested
data Associativity a = L a | R a | NoAssociativity a
                     deriving (Show, Eq, Functor)

-- | Transform an OpTree using the given function.
foldTree :: (a -> b -> b -> b) -> OpTree a b -> b
foldTree fold (Term b) = b
foldTree fold (Op a op b) =
    fold op (foldTree fold a) (foldTree fold b)

-- | Return a parser such that: given 'op s a', if s matches, the parser 
-- | returns a.
op :: String -> a -> ReadP a
op str a = do
    string str
    return a

termP :: ReadP b -> ReadP (OpTree a b)
termP termp = do
    val <- termp
    skipSpaces
    return $ Term val
    
opP :: ReadP a -> ReadP (OpTree a b -> OpTree a b -> OpTree a b)
opP opp = do
    op <- opp
    skipSpaces
    return $ (\opr1 opr2 -> Op opr1 op opr2)

-- | Accept two arguments: 
-- | (1) A list of type [Associativity [ReadP a]], which contains parsers for
-- | operators (ReadP a). Each item of type Associativity [ReadP a] contains
-- | a group of operator parsers of the same precedence and associativity; 
-- | these groups are listed in order of precedence (lowest to highest).
-- | (2) A parser for the terms.
-- | And return a parser for operator expressions that yields a parse tree. 
parseOperators'' :: [Associativity [ReadP a]] -> [Associativity [ReadP a]] -> ReadP b -> ReadP (OpTree a b)
parseOperators'' allop [] termp = termP termp
parseOperators'' allop (as:rest) termp = do
    case as of
        L ps -> chainl1 (parseOperators' allop rest termp) (opP $ choice ps)
        R ps -> chainr1 (parseOperators' allop rest termp) (opP $ choice ps)
        NoAssociativity ps -> (do
            t1 <- parseOperators' allop rest termp
            f <- opP $ choice ps
            t2 <- parseOperators' allop rest termp
            return $ f t1 t2) <|> parseOperators' allop rest termp

parseOperators' :: [Associativity [ReadP a]] -> [Associativity [ReadP a]] -> ReadP b -> ReadP (OpTree a b)
parseOperators' allop ops termp =
    (do
        char '('; skipSpaces
        tree <- parseOperators'' allop allop termp
        char ')'; skipSpaces
        return tree) <|> parseOperators'' allop ops termp

parseOperators :: [Associativity [ReadP a]] -> ReadP b -> ReadP (OpTree a b)
parseOperators ops termp = parseOperators' ops ops termp

doParse :: ReadP a -> String -> a
doParse parser str = case readP_to_S parser str of
    [] -> error "Hugh?"
    -- [(val, rest)] -> error rest
    -- [(v1, r1), (v2, r2), (v3, r3)] -> v2
    l -> case last l of
        (val, []) -> val
        _ -> error "Hugh?"
            
arithOps :: [Associativity [ReadP String]]
arithOps = 
    map (fmap (map (\s -> op s s) . words)) 
        [ R "&& ||", NoAssociativity "< > =", L "+ -", L "* /", R "^"]

arithParser :: String -> String
arithParser s = 
    case readP_to_S (parseOperators arithOps (munch1 isDigit) <* eof) s of
        [] -> ""
        xs -> brackets $ fst (last xs)

brackets :: OpTree String String -> String
brackets = foldTree (\o x y -> '(' : x ++ o ++ y ++ ")")
        