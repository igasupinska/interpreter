module PPrint where

writeln :: String -> IO ()
writeln = putStrLn

-- Iga: ShowS = String -> String
showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v) = ((k ++ ": " ++ show v) ++)

pprH, pprV :: [ShowS] -> ShowS
pprV l = intercalateS ("\n" ++) l
pprH l = intercalateS ("_" ++) l

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep l = ((intercalateSHelper sep l) ++)

intercalateSHelper :: ShowS -> [ShowS] -> String
intercalateSHelper sep [l] = sep (l "")
intercalateSHelper sep (l:ls) = sep (l (intercalateSHelper sep ls))

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith f l = ((pprListWithHelper f l) ++)

pprListWithHelper :: (a -> ShowS) -> [a] -> String
pprListWithHelper f [] = ""
pprListWithHelper f (l:ls) = f l (pprListWithHelper f ls)

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
