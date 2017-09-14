module EscapeTheMines where

import Debug.Trace (trace)

type Map = [[Bool]]
type XY = (Int, Int)
data Move = U | D | R | L deriving (Eq, Show)

legal :: Map -> XY -> Bool
legal grid pos =
    let
        (x, y) = pos
        w = length $ grid !! 0
    in
        x >= 0 && x < length grid && y >= 0 && y < w

get :: Map -> XY -> Bool
get grid pos =
    let
        (x, y) = pos
    in
        grid !! x !! y

set :: Map -> XY -> Map
set grid pos =
    let
        (x, y) = pos
    in
        map (\lst ->
            let
                (i, row) = lst
            in
                if i == x then
                    map (\col ->
                        if fst col == y then True
                        else snd col
                    ) $ zip [0..] row
                else row
        ) $ zip [0..] grid

addmov :: XY -> XY -> XY
addmov pos mov =
    let
        (x1, y1) = pos
        (x2, y2) = mov
    in
        (x1 + x2, y1 + y2)

allfalse :: Map -> Map
allfalse grid = map (map (\x -> False)) grid

dirs = [ (0, -1, U), (0, 1, D), (-1, 0, L), (1, 0, R) ]

solve' :: Map -> XY -> XY -> XY -> Map -> [Move] -> [Move]
solve' grid miner exit cur reached moves =
    if not $ get grid cur then []
    else if cur == exit then moves
    else if get reached cur then []
    else
        let
            ngrid = set reached cur
        in
            foldl (\acc mov ->
                let
                    (x, y, dir) = mov
                    m = addmov cur (x, y)
                in
                    if acc /= [] then acc
                    else if legal grid m then
                        let
                            nmoves = moves ++ [ dir ]
                            res = solve' grid miner exit m ngrid nmoves
                        in
                            if res == [] then []
                            else res
                    else []
            ) [] dirs

--solve' grid miner exit m ngrid nmoves

solve :: Map -> XY -> XY -> [Move]
solve grid miner exit = solve' grid miner exit miner (allfalse grid) []
