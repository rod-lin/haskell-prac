module Huffman (frequencies, encode, decode, Bit (..)) where

import Data.List
import Data.Map
import Debug.Trace (trace)

data Bit = Z | O deriving (Eq, Show)
data HTree n = Node n (HTree n) | Last n n | Null deriving (Eq, Show)

-- | Calculate symbol frequencies of a text.
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies text = toList $ snd $ (!! (length text)) $ iterate (\val ->
    let
        (text, dict) = val
        c = text !! 0
    in
        case Data.Map.lookup c dict of
            Just num -> (drop 1 text, Data.Map.insert c (num + 1) dict)
            Nothing -> (drop 1 text, Data.Map.insert c 1 dict)
    ) (text, fromList [])
    
sortFreq :: Ord a => [(a, Int)] -> [(a, Int)]
sortFreq = (reverse . (Data.List.sortBy (\a b -> compare (snd a) (snd b))))

-- freq sorted(in descending order)
toHTree :: Ord a => [(a, Int)] -> HTree (a, Int)
toHTree ([a, b]) = Last a b
toHTree (maxf:rest) = Node maxf (toHTree rest)
toHTree [] = Null

encodingOf :: Ord a => HTree (a, Int) -> [Bit] -> a -> Maybe [Bit]
encodingOf htree cur c =
    case htree of
        Node (val, freq) right ->
            if val == c then Just (cur ++ [O])
            else encodingOf right (cur ++ [Z]) c
            
        Last (v1, f1) (v2, f2) ->
            if v1 == c then
                Just (cur ++ [O])
            else if v2 == c then
                Just (cur ++ [Z])
            else
                Nothing
                
        Null -> Nothing
                
decodingOf :: Ord a => HTree (a, Int) -> HTree (a, Int) -> [Bit] -> Maybe [a] -> Maybe [a]
decodingOf _ _ _ Nothing = Nothing
decodingOf (Node (val, _) right) orig (c:rest) (Just cur_res) =
    if c == O then
        -- reset for the next character
        decodingOf orig orig rest (Just $ cur_res ++ [val])
    else
        decodingOf right orig rest (Just cur_res)
        
-- decodingOf (Node left right) orig [] res = if (Node left right) == orig then res else Nothing

decodingOf (Last (v1, _) (v2, _)) orig (c:rest) (Just cur_res) =
    decodingOf orig orig rest (Just $ cur_res ++ [if c == O then v1 else v2])

decodingOf Null _ _ _ = Nothing

-- nothing left to decode
decodingOf a b [] res
    | a == b = res
    | True = Nothing

-- | Encode a sequence using the given frequencies.
encode :: Ord a => [(a, Int)] -> [a] -> Maybe [Bit]
encode freq text = let
        tree = toHTree $ sortFreq freq
    in
        case tree of
            Null -> Nothing
            Node _ Null -> Nothing
            _ ->
                case sequence $ Data.List.map (encodingOf tree []) text of
                    Just lst -> Just $ concat lst
                    Nothing -> Nothing

-- | Decode a bit sequence using the given frequencies.
decode :: Ord a => [(a, Int)] -> [Bit] -> Maybe [a]
decode freq code = let
        tree = toHTree $ sortFreq freq
    in
        case tree of
            Null -> Nothing
            Node _ Null -> Nothing
            _ -> decodingOf tree tree code (Just [])
