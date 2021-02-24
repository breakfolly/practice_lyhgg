import System.IO

-- main = do
--     handle <- openFile "girlfriend.txt" ReadMode
--     contents <- hGetContnets handle
--     putStr contents
--     hClose handle

main = do
    withFile "girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
