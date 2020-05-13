-- based on Main module generated automatically
-- by BNF converter as an example

import AbsGramm
import LexGramm
import ParGramm
import ErrM

import TypeChecker
import Interpreter
import Types

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad.IO.Class


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "No file to parse...\n"
        [file] -> parseFile file
        files -> putStrLn "Too many files to parse...\n"

parseFile :: String -> IO ()
parseFile fileName = readFile fileName >>= parse

parse :: String -> IO ()
parse input = let src = myLexer input in
        case pProgram src of
            Bad c ->  do
                putStrLn "Parse Failed...\n"
                exitFailure
            Ok c  ->  do
                checkRes <- checkProg c
                case checkRes of
                    Left err -> do
                        hPutStrLn stderr (show err)
                        exitFailure
                    Right () -> do
                        res <- runProg c
                        case res of
                            Left err -> do
                                hPutStrLn stderr (show err)
                                exitFailure
                            Right (val, store) -> do
                                exitSuccess