import Data.List (sort)
import Data.Char (ord)

scramble :: [Char] -> [Char] -> Bool
scramble s1 s2 =
    let
        l1 = length s1
        l2 = length s2
        ss1 = (sort . map ord) s1
        ss2 = (sort . map ord) s2
    in
        case compare l1 l2 of
            LT -> False
            EQ -> ss1 == ss2
            GT ->
                let
                    (_, _, k) = (last (take (l1 + l2) (iterate (\val ->
                            let
                                (i, j, k) = val
                            in
                                if i < l1 && j < l2 then
                                    case compare (ss1 !! i) (ss2 !! j) of
                                        LT -> (i + 1, j, k)
                                        GT -> (i, j + 1, k)
                                        EQ -> (i + 1, j + 1, k + 1)
                                else val
                        ) (0, 0, 0))))
                in k == l2
