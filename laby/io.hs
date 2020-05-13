import System.Environment

-- -- 3.1
-- main :: IO()
-- main = do
--     result <- getArgs
--     putStrLn $ printArgs result

-- printArgs :: [String] -> String
-- printArgs [] = []
-- printArgs [s] = s
-- printArgs (s:ss) = s ++ "\n" ++ printArgs ss

-- -- 3.2
-- main = do
--     putStrLn "What's your favourite programming language?"
--     lang <- getLine
--     if lang == "Haskell"
--         then putStrLn lang
--         else main

-- 3.3
-- Napisz uproszczoną wersję programu wc
-- (wypisującą ilość linii, słów i znaków w pliku o nazwie podanej
-- jako argument, bądź stdin jeśli bez argumentu)
main = do
    content <- getContents
    if content == []
        then putStrLn "stdin"
        else let (l, w, c) = count content (0, 0, 0) in
            putStrLn $ "lines: " ++ show l
                    ++ " words: " ++ show w
                    ++ " chars: " ++ show c

count :: String -> (Int, Int, Int) -> (Int, Int, Int)
count [] (l, w, c) = (l+1, w+1, c)
count (s:ss) (l, w, c)
    | s == ' '  = count ss (l, w+1, c+1)
    | s == '\n' = count ss (l+1, w+1, c+1)
    | otherwise = count ss (l, w, c+1)
