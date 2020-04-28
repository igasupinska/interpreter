import AbsGramm
import LexGramm
import ParGramm
import ErrM

import Interpreter

import System.IO (stdin, hGetContents, hPutStrLn, stderr, getContents, hPutStr)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import Control.Exception (catch, IOException)

printResult :: (StoredVal, Store) -> String
printResult ((SInt res), store) = show res 
printResult ((SBool res), store) = show res 
printResult ((SStr res), store) = show res

main :: IO () --Iga: skopiowane
main = do
    args <- getArgs
    case args of
        [] -> getContents >>= parse
        files -> mapM_ parseFile files

parseFile :: String -> IO () --Iga: skopiowane
parseFile fileName = readFile fileName >>= parse

parse :: String -> IO () --Iga: skopiowane
parse input = let src = myLexer input in
        case pProgram src of
            Bad c ->  do
                        putStrLn "Parse Failed...\n"
                        exitFailure
            Ok c  ->  do
                        putStrLn "Parse Successful!\n"
                        putStrLn $ printResult $ runProg c
                        exitSuccess


-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     ["--help"] -> usage
--     [] -> hGetContents stdin >>= run 2 pProgram
--     "-s":fs -> mapM_ (runFile 0 pProgram) fs
--     fs -> mapM_ (runFile 2 pProgram) fs

-- runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
-- runFile v p f = putStrLn f >> readFile f >>= run v p

-- run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
-- run v p s = let ts = myLLexer s in case p ts of
--            Bad s    -> do putStrLn "\nParse              Failed...\n"
--                           putStrV v "Tokens:"
--                           putStrV v $ show ts
--                           putStrLn s
--                           exitFailure
--            Ok  tree -> do putStrLn "\nParse Successful!"
--                           showTree v tree

--                           exitSuccess
