
data Expression = Add Expression Expression | Sub Expression Expression | Const Float deriving (Show, Eq)

eval :: Expression -> Float
eval (Const v) = v
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b

data TypeA = ValA {
        name :: String
    } deriving (Show)

data Container a = NULL | Val a deriving (Show)

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
