module TryParser where

import Data.Char
import Data.List
import Control.Monad
import Control.Applicative

import Debug.Trace

-- original code by ice1000 http://ice1000.org/2017/07/26/HaskellParsers/,
-- commented and edited

-- Parser type
newtype Parser val = Parser { parse :: String -> [(val, String)] }

-- Parser is a functor
instance Functor Parser where
    -- maps parsed results and returns a new parser
    fmap f (Parser ps) = Parser $ \p -> [ (f a, b) | (a, b) <- ps p ]
    -- <$> = fmap

instance Applicative Parser where
    pure = return
    -- the string goes through p1 first,
    -- then the rest is passed to p2
    -- and apply the second result(a) to the first result(f)
    (Parser p1) <*> (Parser p2) = Parser $ \p ->
        [ (f a, s2) | (f, s1) <- p1 p, (a, s2) <- p2 s1 ]

instance Monad Parser where
    return a = Parser $ \s -> [(a, s)]
    -- bind p to f
    -- use p first to parse the input string
    -- then pass every single result(unwrapped) to f and result a bunch of new results
    -- concatMap is used to combine all possibilities
    p >>= f  = Parser $ concatMap (\(a, s1) -> f a `parse` s1) . parse p

instance MonadPlus Parser where
    mzero     = Parser $ const []
    -- simply concat the results
    mplus p q = Parser $ \s -> parse p s ++ parse q s

instance Alternative Parser where
    empty   = mzero
    -- kind of acts like the logical or in c
    p <|> q = Parser $ \s -> case parse p s of
        [] -> parse q s -- first result empty(parsing failed) -> result the second result
        rs -> rs -- first true

item :: Parser Char
item = Parser $ \s -> case s of
    [     ] -> []
    (h : t) -> [(h, t)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c -> if p c then return c else empty

doParse :: Parser a -> String -> a
doParse m s = case parse m s of
    -- one result and no char left
    [(res, [])] -> res
    [(res, rest)] -> trace rest (error "??")
    _           -> error "Hugh?"

charP :: Char -> Parser Char
charP c = satisfy (== c)

tokenP :: String -> Parser String
tokenP [] = return []
tokenP str@(c:rest) = do
    charP c
    tokenP rest
    return str

-- some -> 1+
-- many -> 0+
-- some, many :: f a -> f [a]
-- some v = (:) <$> v <*> many v
-- many v = some v <|> pure []

oneOfP :: String -> Parser Char
oneOfP str = satisfy (`elem` str)

numberP :: Parser Int
numberP = (do
    first <- satisfy (`elem` ['1'..'9'])
    rest <- many $ satisfy isDigit
    return $ read (first:rest)) <|> (do charP '0'; return 0)
    
space0P :: Parser String
space0P = many $ oneOfP " \n\r\t"

space1P :: Parser String
space1P = some $ oneOfP " \n\r\t"

nl1P :: Parser String
nl1P = some $ oneOfP "\n\r"

-- left associative op chain
chainl0P :: Parser (a -> a -> a) -> a -> Parser a -> Parser a
chainl0P opp lval rvalp = (do
    op <- opp
    rval <- rvalp
     -- rest of the chain
    chainl0P opp (op lval rval) rvalp) <|> pure lval
    
chainl1P :: Parser (a -> a -> a) -> Parser a -> Parser a -> Parser a
chainl1P opp lvalp rvalp = do
    lval <- lvalp
    chainl0P opp lval rvalp

-- right associative op chain
chainr1P :: Parser (a -> a -> a) -> Parser a -> Parser a -> Parser a
chainr1P opp lvalp rvalp = do
    lval <- lvalp
    op <- opp
    rval <- (chainr1P opp lvalp rvalp) <|> rvalp
    return $ op lval rval
    
data AST = Int Int | Op String AST AST deriving (Show)

ast'SimpleOpP :: String -> Parser (AST -> AST -> AST)
ast'SimpleOpP opstr = do
    space0P; tokenP opstr; space0P
    return $ Op opstr
    
ast'IntP :: Parser AST
ast'IntP = do
    int <- numberP
    return $ Int int
