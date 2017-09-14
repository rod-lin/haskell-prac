module TextAlignJustify where

swap :: (a -> b -> c) -> (b -> a -> c)
swap f = (\x y -> f y x)

fillwidth :: [String] -> Int -> String
fillwidth lst width =
    let
        gapcount = length lst - 1
        wlen = length (concat lst)
        mingap = fromEnum (floor ((fromIntegral (width - wlen)) / fromIntegral gapcount))
        gap = [ ' ' | _ <- [ 1 .. mingap ] ]
        longgap = width - mingap * gapcount - wlen

        gapped = (map (++ gap) . take gapcount) lst
    in
        concat ((map (++ " ") . take longgap) gapped ++ (drop longgap) gapped) ++ last lst ++ "\n"

fitline :: [String] -> Int -> (Int, String)
fitline lst width =
    let
        wordlst = (last . takeWhile ((<= width) . length . unwords) . scanl (\acc x -> acc ++ [x]) []) lst
    in
        if wordlst == lst then
            ((length wordlst), unwords wordlst)
        else
            ((length wordlst), fillwidth wordlst width)

justify' :: [String] -> Int -> String
justify' [] width = ""
justify' lst width =
    let
        (used, ret) = fitline lst width
    in
        ret ++ justify' (drop used lst) width

justify :: String -> Int -> String
justify text width = justify' (words text) width
