module MoleculeToAtoms where

import Data.Char
import Debug.Trace (trace)

parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula = let
        (count, parsed) = toast formula
    in if count /= length formula then
        Left "syntax error"
    else case parsed of
        Err msg -> Left msg
        _ -> Right $ countAtom parsed

data AST = Atom String | Multi AST Int | Combine AST AST | Empty | Err String deriving (Show)

allparen = "([{)]}"
parencount = 3 :: Int

indexOf :: String -> Char -> Int
indexOf str c = let
        i = length $ takeWhile (/= c) str
    in if i == length str then -1 else i

parseCount :: String -> Int
parseCount num = if num == "" then 1 else read num

parseParen :: String -> Char -> (Int, AST)
parseParen rest rparen = let
        (count1, ast1) = toast rest
        rrest = drop count1 rest
    in case ast1 of
        Err msg -> (0, ast1)
        Empty -> (0, Err "syntax error")
        _ ->
            if length rrest /= 0 && rrest !! 0 == rparen then
                let
                    rrrest = drop 1 rrest
                    numsuf = takeWhile isDigit rrrest
                    
                    numlen = length numsuf
                
                    rrrrest = drop (length numsuf) rrrest
                
                    num = parseCount numsuf
                    mol = if num == 1 then ast1 else Multi ast1 num
                
                    (count2, ast2) = toast rrrrest
                in case ast2 of
                    Err msg -> (0, ast2)
                    Empty -> (count1 + count2 + numlen + 2, mol)
                    other -> (count1 + count2 + numlen + 2, Combine mol ast2)
            else
                (0, Err "syntax error")

toast :: String -> (Int, AST)

toast ('(':rest) = parseParen rest ')'
toast ('[':rest) = parseParen rest ']' 
toast ('{':rest) = parseParen rest '}'
        
toast (c:rest)
    | isUpper c = let
            name = c:(takeWhile isLower rest)
            numsuf = takeWhile isDigit (drop (length name - 1) rest)
            len = length name + length numsuf
            
            num = parseCount numsuf
            
            atom = if num == 1 then Atom name else Multi (Atom name) num
            
            rrest = drop (len - 1) rest
            (count1, ast1) = toast rrest
        in case ast1 of
            Err msg -> (0, ast1)
            Empty -> (len, atom)
            _ -> (len + count1, Combine atom ast1)
    | True = (0, Empty)

toast _ = (0, Empty)

findAtom :: [(String, Int)] -> String -> Int
findAtom dict name = if find == [] then 0 else snd $ find !! 0
    where find = filter ((== name) . fst) dict

countAtom :: AST -> [(String, Int)]
countAtom (Multi mol num) = map (\x -> (fst x, snd x * num)) $ countAtom mol
countAtom (Atom name) = [(name, 1)]
countAtom (Combine mol1 mol2) = let
        map1 = countAtom mol1
        map2 = countAtom mol2
        
        map1_inc = map (\x -> let
                (name, count) = x
                count2 = findAtom map2 name
            in (name, count + count2)) map1
        
        map2_red = filter ((== 0) . (findAtom map1) . fst) map2
    in
        map1_inc ++ map2_red

countAtom Empty = []
