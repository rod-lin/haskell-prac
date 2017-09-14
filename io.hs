import System.IO
import System.Environment
import Debug.Trace (trace)

plus1 = interact $ unlines . map (show . (+ 1) . read) . lines

cat = do
    lst <- getArgs
    if length lst /= 0 then do
        fp <- openFile (head lst) ReadMode
        cont <- hGetContents fp
        putStrLn cont
        hClose fp
    else do
        putStrLn "no file given"

main = cat
