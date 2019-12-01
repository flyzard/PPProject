module Main where

import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
import Data.List  
import Gafipf.Percursos

main :: IO ()
main = toTry `catch` handler

toTry :: IO ()  
toTry = do (percursoPath:pontosPath:jsonPath:_) <- getArgs
           percurso <- readFile percursoPath
           pontos <- readFile pontosPath
           buildJson percurso pontos jsonPath
           
  
handler :: IOError -> IO ()     
handler e     
    | isDoesNotExistError e =   
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path  
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"  
    | otherwise = ioError e   


buildJson :: String -> String -> IO ()
buildJson percurso pontos jsonPath = writeFile jsonPath toJson buildPercurso percurso pontos

writeJsonFile :: String -> String -> IO () -- Check if file exists and throw an error
writeJsonFile jsonPath content = writeFile jsonPath content
