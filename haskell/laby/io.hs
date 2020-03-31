import System.Environment

main :: IO()
main = do
    result <- getArgs
    putStrLn $ printArgs result

printArgs :: [String] -> String
printArgs [] = []
printArgs [s] = s
printArgs (s:ss) = s ++ "\n" ++ printArgs ss