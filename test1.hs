import Data.List.Split (splitOn)

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

pow :: (Eq a, Num a) => a -> a -> a
pow x 0 = 1
pow x 1 = x
pow x n = x * pow x (n - 1)

len' [] = 0
len' (x:xs) = 1 + len' xs

-- xo_tmp :: String -> Int
-- xo_tmp [] = 0
-- xo_tmp ('o':str) = 1 + xo_tmp str
-- xo_tmp ('x':str) = (-1) + xo_tmp str
-- xo_tmp (c:str) = xo_tmp str

-- | Returns true if the number of
-- Xs is equal to the number of Os
-- (case-insensitive)
xo :: String -> Bool
xo str =
    foldl
    (\ acc c ->
        acc + (if c == 'o' || c == 'O' then 1
               else if c == 'x' || c == 'X' then -1
               else 0))
    0 str == 0

toint :: String -> Int
toint "zero" = 0
toint "one" = 1
toint "two" = 2
toint "three" = 3
toint "four" = 4
toint "five" = 5
toint "six" = 6
toint "seven" = 7
toint "eight" = 8
toint "nine" = 9

tostr :: Int -> String
tostr 0 = "zero"
tostr 1 = "one"
tostr 2 = "two"
tostr 3 = "three"
tostr 4 = "four"
tostr 5 = "five"
tostr 6 = "six"
tostr 7 = "seven"
tostr 8 = "eight"
tostr 9 = "nine"

averageString :: String -> String
averageString str =
  (\lst ->
    if length str > 0 && sum lst >= 0
    then tostr (if sum lst == 0 then 0 else sum lst `quot` length lst)
    else "n/a") (map toint (words str))

vow = "aeiou"

isAlt :: String -> Bool
isAlt (c:[]) = c `elem` vow
isAlt (c:rst) = sum ((\lst -> [
        if mod i 2 == 0 then
            if (lst !! i) `elem` vow then 0 else 1
        else
            if (lst !! i) `elem` vow then 1 else 0
        | i <- [0..length lst - 1]
    ]) (if c `elem` vow then c:rst else rst)) == 0

spinWords :: String -> String
spinWords = unwords . (map (\x -> if length x >= 5 then reverse x else x)) . words
