import Debug.Trace (trace)

-- very slow version
-- count0 :: String -> Int
-- count0 = length . takeWhile (== '0') . reverse
--
-- trim0 :: String -> Int -> Integer
-- trim0 a n = trace (a) (read (take (length a - n) a))
--
-- factzeros :: Integer -> Integer -> Integer -> Int
-- factzeros a b fact =
--     let
--         nfact = show (a * fact)
--         count = count0 nfact
--     in
--         if a > b then
--             0
--         else
--             count + factzeros (a + 1) b (trim0 nfact count)
--
-- zeros :: Int -> Int
-- zeros n = factzeros 1 (fromIntegral n) 1

truncdiv :: Integer -> Integer -> Integer
truncdiv a b = floor (fromInteger a / fromInteger b)

find5mul :: Integer -> Integer -> Integer
find5mul n cur = if cur <= n then truncdiv n cur + find5mul n (cur * 5) else 0

zeros :: Int -> Int
zeros n = fromEnum (find5mul (fromIntegral n) 5)
