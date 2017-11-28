-- Glob pattern matcher

import Data.List
import Debug.Trace (trace)

data MatchElem = MString String |
                 MAny Int |
                 MCharClass Bool String deriving (Show)

type Matcher = [MatchElem]

lastSafe lst = if length lst > 0 then Just (last lst) else Nothing

(opt1, rest1) `charClassAdd` (opt2, rest2) = (opt1 ++ opt2, rest2)

parseCharClass :: String -> Maybe (String, String)
parseCharClass (']':cs) = Just ("", cs)
parseCharClass ('\\':esc:cs) = charClassAdd <$> Just ([esc], cs) <*> parseCharClass cs

parseCharClass (a:'-':b:cs) =
    if a <= b then
        charClassAdd <$> Just ([a..b], cs) <*> parseCharClass cs
    else Nothing

parseCharClass (c:cs) = charClassAdd <$> Just ([c], cs) <*> parseCharClass cs
parseCharClass [] = Nothing

pushMatchElem cur c =
    case lastSafe cur of
        Just (MString str) -> (init cur) ++ [MString (str ++ [c])]
        _ -> cur ++ [MString [c]]

parseMatcher' :: Matcher -> String -> Maybe Matcher
parseMatcher' cur ('\\':esc:cs) = parseMatcher' (pushMatchElem cur esc) cs
parseMatcher' cur ('*':cs) = parseMatcher' (cur ++ [MAny 0]) cs
parseMatcher' cur ('?':cs) = parseMatcher' (cur ++ [MAny 1]) cs

parseMatcher' cur ('[':c:cs) =
    let
        contain = c /= '!'
        res = parseCharClass (if contain then c:cs else cs)
    in case res of
        Just (opts, rest) -> parseMatcher' (cur ++ [MCharClass True opts]) rest
        Nothing -> Nothing

parseMatcher' cur (c:cs) = parseMatcher' (pushMatchElem cur c) cs

parseMatcher' cur [] = Just cur

parseMatcher :: String -> Maybe Matcher
parseMatcher = parseMatcher' []

match :: Matcher -> String -> Bool
match ((MString pref):rest) str =
    if isPrefixOf pref str then match rest (drop (length pref) str)
    else False

match ((MAny count):rest) str =
    if count == 1 then
        if length str > 0 then match rest (drop 1 str)
        else False
    else
        (length $ dropWhile (not . match rest . (`drop` str) ) [0..length str]) > 0

match ((MCharClass contain opts):rest) (c:cs) =
    if elem c opts == contain then
        match rest cs
    else False

match [] [] = True
match _ _ = False

glob :: String -> String -> Maybe Bool
glob pattern str = (`match` str) <$> (parseMatcher pattern)
