import Data.List (sortBy, sort)
import Data.Function (on)
import Data.Char (ord, chr)

orderWeight :: [Char] -> [Char]
orderWeight =
    unwords . (map fst) .
    (sortBy (compare `on` snd)) . (map (\x -> (x, sum (map ((\x -> x - ord '0') . ord) x)))) .
    (map (map chr)) . sort . (map (map ord)) . words
