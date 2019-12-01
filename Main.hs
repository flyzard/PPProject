module Main where

import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
import Data.List  


main :: IO ()
main = toTry `catch` handler

toTry :: IO ()  
toTry = do 
    args <- getArgs  
    progName <- getProgName  
    putStrLn "The arguments are:"  
    mapM putStrLn args  
    putStrLn "The program name is:"  
    putStrLn progName 

-- toTry :: IO ()  
-- toTry = do (percursoPath:pontosPath:jsonPath:_) <- getArgs
--            percurso <- readFile percursoPath
--            pontos <- readFile pontosPath
--            putStrLn $ "The file has " ++ show (length (lines percurso)) ++ " lines!"
  
handler :: IOError -> IO ()     
handler e     
    | isDoesNotExistError e =   
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path  
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"  
    | otherwise = ioError e   
