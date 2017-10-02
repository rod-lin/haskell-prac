module LispLovesMe where

import Data.Char
import Data.List
import Control.Monad
import Control.Applicative

import Debug.Trace

-- Parser start
-- original code by ice1000 http://ice1000.org/2017/07/26/HaskellParsers/

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

doParse :: Parser a -> String -> Maybe a
doParse m s = case parse m s of
    -- one result and no char left
    [(res, [])] -> Just res
    _           -> Nothing

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

noneOfP :: String -> Parser Char
noneOfP str = satisfy (not . (`elem` str))

-- Parser end

-- Lisp

data AST = I32 Int
         | Sym String
         | Nul
         | Err
         | Lst [AST]
         | Boo Bool
         | Nod AST [AST]
         deriving (Eq, Show)

preludeFunctions :: [(String, [AST] -> AST)]
preludeFunctions =
    [ ("+", preludeAdd)
    , ("*", preludeMul)
    , ("-", preludeSub)
    , ("/", preludeDiv)
    , ("^", preludeExp)
    , (">", preludeGT)
    , ("<", preludeLT)
    , ("!", preludeNot)
    , ("list", preludeList)
    , ("size", preludeSize)
    , ("reverse", preludeRev)
    , ("..", preludeRange)
    , ("==", preludeEQ)
    , (">=", preludeGE)
    , ("<=", preludeLE)
    , ("!=", preludeNE)
    , ("if", preludeIf)]

maybeMaybeToAST :: (a -> AST) -> Maybe (Maybe a) -> AST
maybeMaybeToAST wrap val = case val of
    Just (Just v) -> wrap v
    Nothing -> Err 

binFoldInt :: (Int -> Int -> Int) -> [Int] -> Maybe Int
binFoldInt op args =
    case args of
        [a] -> Just a
        a:b -> Just $ foldl op a b
        [] -> Nothing

binopI32 :: (Int -> Int -> Int) -> [AST] -> AST
binopI32 op =
    (maybeMaybeToAST I32) .
    (binFoldInt op <$>) .
    sequence .
    (map $
        \x -> case x of
            I32 num -> Just num
            _ -> Nothing)

preludeAdd :: [AST] -> AST
preludeAdd = binopI32 (+)

preludeSub :: [AST] -> AST
preludeSub = binopI32 (-)

preludeMul :: [AST] -> AST
preludeMul = binopI32 (*)

preludeDiv :: [AST] -> AST
preludeDiv = binopI32 div

preludeExp :: [AST] -> AST
preludeExp [I32 a, I32 b] = I32 $ a ^ b
preludeExp _ = Err

preludeGT :: [AST] -> AST
preludeGT [I32 a, I32 b] = Boo $ a > b
preludeGT _ = Err

preludeGE :: [AST] -> AST
preludeGE [I32 a, I32 b] = Boo $ a >= b
preludeGE _ = Err

preludeLT :: [AST] -> AST
preludeLT [I32 a, I32 b] = Boo $ a < b
preludeLT _ = Err

preludeLE :: [AST] -> AST
preludeLE [I32 a, I32 b] = Boo $ a <= b
preludeLE _ = Err

preludeEQ :: [AST] -> AST
preludeEQ [I32 a, I32 b] = Boo $ a == b
preludeEQ _ = Err

preludeNE :: [AST] -> AST
preludeNE [I32 a, I32 b] = Boo $ a /= b
preludeNE _ = Err

preludeNot :: [AST] -> AST
preludeNot [Boo a] = Boo $ not a 
preludeNot _ = Err

preludeList :: [AST] -> AST
preludeList list = Lst list

preludeRev :: [AST] -> AST
preludeRev [Lst list] = Lst $ reverse list
preludeRev _ = Err

preludeSize :: [AST] -> AST
preludeSize [Lst list] = I32 $ length list
preludeSize _ = Err

preludeRange :: [AST] -> AST
preludeRange [I32 a, I32 b] = Lst $ map I32 [a..b]
preludeRange _ = Err

preludeIf :: [AST] -> AST
preludeIf [Boo cond, a, b] = if cond then a else b
preludeIf [Boo cond, a] = if cond then a else Nul
preludeIf _ = Err

-- syntax

lispSpace0P :: Parser String
lispSpace0P = many $ oneOfP " \n\r\t,"

lispSpace1P :: Parser String
lispSpace1P = some $ oneOfP " \n\r\t,"

lispI32P :: Parser AST
lispI32P = do
    v <- oneOfP "-+" <|> pure '+'
    val <- numberP
    return $ if v == '+' then I32 val else I32 (-val)

lispSymbolP :: Parser AST
lispSymbolP = do
    c1 <- noneOfP " ,\n\t\r()0123456789"
    cs <- many $ noneOfP " ,\n\t\r()"
    return $ Sym $ c1:cs

lispBoolP :: Parser AST
lispBoolP = (const (Boo False) <$> tokenP "false")
        <|> (const (Boo True) <$> tokenP "true")

lispNullP :: Parser AST
lispNullP = const Nul <$> (tokenP "null" <|> (do charP '('; lispSpace0P; charP ')'; return "()"))

lispNodeP :: Parser AST
lispNodeP = do
    lispSpace0P; charP '('; lispSpace0P
    callee <- lispExprP
    args <- lispExprListP
    lispSpace0P; charP ')'
    return $ Nod callee args

lispExprP :: Parser AST
lispExprP = do
    lispSpace0P;
    lispI32P <|> lispNullP <|> lispBoolP <|> lispSymbolP <|> lispNodeP

-- list of lisp expression with possible preceding spaces
lispExprListP :: Parser [AST]
lispExprListP = (do
    expr <- lispExprP
    rest <- lispExprListP
    return $ expr:rest) <|> pure []
    
lispASTP :: Parser AST
lispASTP = do
    ast <- lispExprP
    lispSpace0P
    return ast

lispEvalAST :: AST -> AST
lispEvalAST (Nod callee args) = let
        v = lispEvalAST callee
        a = map lispEvalAST args
    in case v of
        Sym name ->
            case lookup name preludeFunctions of
                Just func -> func a
                Nothing -> Err -- error "symbol not found"
        _ -> Err -- error "non-applicable value"

lispEvalAST other = other

lispEval :: String -> Maybe AST
lispEval s = case doParse lispASTP s of
    Just ast -> Just $ lispEvalAST ast
    Nothing -> Nothing

lispPrettyAST :: AST -> String
lispPrettyAST (Nod callee args) =
    "(" ++ lispPrettyAST callee ++ concat (map ((" " ++) . lispPrettyAST) args) ++ ")"

lispPrettyAST (Boo True) = "true"
lispPrettyAST (Boo False) = "false"
lispPrettyAST (I32 a) = show a
lispPrettyAST (Sym a) = a
lispPrettyAST Nul = "null"

lispPretty :: String -> Maybe String
lispPretty s = case doParse lispASTP s of
    Just ast -> Just $ lispPrettyAST ast
    Nothing -> Nothing
