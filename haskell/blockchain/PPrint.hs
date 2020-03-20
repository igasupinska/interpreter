module PPrint where

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v) = ((k ++ ": " ++ show v) ++)

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS ("\n" ++)
pprH = intercalateS (" " ++)

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep (l:ls) = (l (intercalateSHelper sep ls) ++)

intercalateSHelper :: ShowS -> [ShowS] -> String
intercalateSHelper sep [] = []
intercalateSHelper sep (l:ls) = sep (l (intercalateSHelper sep ls))

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith f (l:ls) = ((f l) (pprListWithHelper f ls) ++)

pprListWithHelper :: (a -> ShowS) -> [a] -> String
pprListWithHelper f [] = []
pprListWithHelper f (l:ls) = "\n" ++ f l (pprListWithHelper f ls)

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
