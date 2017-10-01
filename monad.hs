
try = do
    im <- Just "happy"
    so <- Just "play"
    tired <- Just "go away"
    return $ im ++ so ++ tired
