
divisors :: Int -> [Int]
divisors n = (filter (\x -> n `mod` x == 0) . takeWhile (<=n)) [ 1 .. ]

square :: [Int] -> [Int]
square lst = (map (fromEnum . floor . (** 2) . fromIntegral)) lst

issquare :: Int -> Bool
issquare n = (fromEnum . (** 2) . fromIntegral . floor . sqrt . fromIntegral) n == n

listSquared :: Int -> Int -> [(Int, Int)]
listSquared m n = (filter (issquare . snd) . map (\x -> (x, (sum . square . divisors) x))) [ m .. n ]

-- listSquared m n = (filter (issquare . toInteger . snd) . map (\x -> (fromEnum x, (fromEnum (sum (sqdivisor (toInteger x))))))) [m .. n]
