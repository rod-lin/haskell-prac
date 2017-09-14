module Awesome.Numbers where

import Debug.Trace (trace)

data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)

isInc :: String -> Bool
isInc str = let len = length str in snd $ foldl (\acc x ->
        let
            (i, ret) = acc
        in
            -- trace (show ret) $
            if i + 1 == len then
                (i + 1, ret)
            else let nextc = str !! (i + 1) in
                (i + 1, ret && (succ x == nextc || (i + 2 == len && x == '9' && nextc == '0')))
    ) (0, True) str

isDec :: String -> Bool
isDec str = if last str == '0' then isInc $ reverse str else isInc $reverse str

isPalindrome :: String -> Bool
isPalindrome str =
    let
        n = floor $ (fromIntegral $ length str) / 2
    in
        take n str == (take n $ reverse str)

isStrictInteresting :: Integer -> [Integer] -> Bool
isStrictInteresting x phs =
    let
        str = show x
        len = length str
        first = head str
    in
        x > 99 && any (== True)
            [
                -- 100, 90000
                all (== '0') $ tail str,

                -- 1111, 222222
                all (== first) str,

                isInc str,
                isDec str,
                isPalindrome str,
                x `elem` phs
            ]

isInteresting :: Integer -> [Integer] -> Answer
isInteresting x xs =
    if isStrictInteresting x xs then Yes
    else if isStrictInteresting (x + 1) xs ||
            isStrictInteresting (x + 2) xs then Almost
    else No
