module UnlimitedGameOfLife where

import Debug.Trace (trace)
import Data.List

type Board = [[Int]]
type Pos = (Int, Int)

neighbour_ofs = [ (-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1) ]

legal :: Board -> Pos -> Bool
legal board pos =
    let
        (x, y) = pos
    in
        x >= 0 &&
        y >= 0 &&
        y < length board &&
        x < length (board !! 0)

live :: Board -> Pos -> Int
live board pos =
    let
        (x, y) = pos
    in
        if legal board pos then
            (board !! y) !! x
        else 0

addpos :: Pos -> Pos -> Pos
addpos p1 p2 =
    let
        (x1, y1) = p1
        (x2, y2) = p2
    in
        (x1 + x2, y1 + y2)

neighbours :: Board -> Pos -> (Int -> a) -> [a]
neighbours board pos fn =
    let
        (x, y) = pos
    in
        map ((\pos ->
            -- trace ("hi" ++ show pos) $
            fn $ live board pos
        ) . addpos pos) $ neighbour_ofs

countNeighbours :: Board -> Pos -> Int
countNeighbours board pos =
    sum $ neighbours board pos (\x -> x)

addBorder :: Board -> Board
addBorder board =
    let
        height = length board
        width = length $ board !! 0
        hborder = [ 0 | _ <- [ 1 .. width + 2 ] ]
        vborder = [ 0 | _ <- [ 1 .. height + 2 ] ]
    in
        hborder :
            (take height $ transpose $ vborder : (transpose board) ++ [vborder])
        ++ [hborder]

trimBorder :: Board -> Board
trimBorder board =
    let
        w = length $ board !! 0
        hborder = [ 0 | _ <- [ 1 .. w ] ]
    in
        transpose .
        (\orig ->
            let
                b = transpose orig
                vborder = [ 0 | _ <- [ 1 .. length orig ] ]
            in
                (\b -> if last b == vborder then take (length b - 1) b else b) .
                (\b -> if head b == vborder then tail b else b) $ b) .
        (\b -> if last b == hborder then take (length b - 1) b else b) .
        (\b -> if head b == hborder then tail b else b) $ board

trimAllBorders :: Board -> Board
trimAllBorders board =
    let
        res = trimBorder board
    in
        if res == board then res else trimAllBorders res

shape :: Board -> (Int, Int)
shape board = (length $ board !! 0, length board)

onepass :: Board -> Int -> Board
onepass [[]] gen = [[]]
onepass orig gen =
    let
        board = addBorder orig
    in
        trimAllBorders $ map (\row -> map (\col ->
            let
                (j, _) = row
                (i, _) = col
                c = countNeighbours board (i, j)
            in
                case c of
                    2 -> live board (i, j)
                    3 -> 1
                    _ -> 0
        ) $ zip [0..] $ snd row) $ zip [0..] $ board

getGeneration :: Board -> Int -> Board
getGeneration board gen = foldl onepass board [1..gen]

-- htmlize :: [[Int]] -> String
-- htmlize = concatMap ((++ "<br>") . concatMap cell)
--     where cell n = if n > 0 then "&#x2593;&#x2593;" else "&#x2591;&#x2591;"
