module RPN where

rpneval :: String -> Integer
rpneval src = head $ foldl (\stack op ->
        case op of
            "+" -> let (a:b:ret) = stack in (a + b):ret
            "-" -> let (a:b:ret) = stack in (a - b):ret
            "*" -> let (a:b:ret) = stack in (a * b):ret
            "/" -> let (a:b:ret) = stack in (floor $ fromInteger a / fromInteger b):ret
            val -> (read val):stack
    ) [] $ words src
