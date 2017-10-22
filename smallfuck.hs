module Haskell.SylarDoom.Smallfuck where
    
data State = State {
        pc :: Int,
        ptr :: Int,
        tape :: [Bool]
    }
    
data Instr = PLeft | PRight | Flip | Cond Int | Prev Int | Term deriving (Show)
    
iscmd :: Char -> Bool
iscmd c = c `elem` [ '<', '>', '*', '[', ']' ]

inbound :: [a] -> Int -> Bool
inbound arr i = i >= 0 && i < length arr

-- "abc[]]" -> 6
-- "abc]]" -> 4
findmatch' :: String -> Int -> Int -> Int
findmatch' string 0 i = i
findmatch' [] nest i = 0
findmatch' (c:rest) nest i =
    if c == '[' then
        findmatch' rest (nest + 1) (i + 1)
    else if c == ']' then
        findmatch' rest (nest - 1) (i + 1)
    else
        findmatch' rest nest (i + 1)
        
findmatch :: String -> Maybe Int
findmatch string = case findmatch' string 1 0 of
    0 -> Nothing
    a -> Just a

parse :: String -> [Instr]
parse [] = []
parse (c:rest) = case c of
    '<' -> PLeft:parse rest
    '>' -> PRight:parse rest
    '*' -> Flip:parse rest
    '[' -> case findmatch rest of
        Just i -> [Cond i] ++ parse (take (i - 1) rest) ++ [Prev (-i)] ++ parse (drop i rest)
        Nothing -> [Term]
    ']' -> [Term]
    _   -> parse rest

fliptape :: [Bool] -> Int -> [Bool]
fliptape tape i = take i tape ++ [not (tape !! i)] ++ drop (i + 1) tape

execute :: State -> [Instr] -> State
execute state code = let
        State { pc = pc, ptr = ptr, tape = tape } = state
    in
        if inbound code pc && inbound tape ptr then
            let
                nstate = case code !! pc of
                    PLeft -> State { pc = pc + 1, ptr = ptr - 1, tape = tape }
                    PRight -> State { pc = pc + 1, ptr = ptr + 1, tape = tape }
                    Flip -> State { pc = pc + 1, ptr = ptr, tape = fliptape tape ptr }
                    Cond next ->
                        if tape !! ptr then
                            State { pc = pc + 1, ptr = ptr, tape = tape }
                        else
                            State { pc = pc + next + 1, ptr = ptr, tape = tape }
                    Prev prev ->
                        if tape !! ptr then
                            State { pc = pc + prev, ptr = ptr, tape = tape }
                        else
                            State { pc = pc + 1, ptr = ptr, tape = tape }
            in
                execute nstate code
        else
            state

stringtotape :: String -> [Bool]
stringtotape string = map (\c -> if c == '1' then True else False) string

tapetostring :: [Bool] -> String
tapetostring tape = map (\t -> if t then '1' else '0') tape

interpreter :: String -> String -> String
interpreter code tape = let
        State { tape = final } = execute (State {
            pc = 0,
            ptr = 0,
            tape = stringtotape tape
        }) (parse code)
    in
        tapetostring final
