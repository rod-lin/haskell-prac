
validParentheses :: String -> Bool
validParentheses str =
    (foldl (\i c -> case c of
        '(' -> i + 1
        ')' -> if i == 0 then -1000000000 else i - 1
    ) 0 str) == 0
