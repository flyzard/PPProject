module Main where

import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
import Data.List
import Gafipf.Percursos
import Gafipf.CsvParser
import Gafipf.Coordenadas
import Gafipf.Poi

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

buildJson :: String -> String -> String -> IO ()
buildJson percurso pontos jsonPath = writeFile jsonPath (toJson (buildPercurso percurso pontos))

buildPercurso :: String -> String -> Percurso
buildPercurso c p = criaPercurso (buildCoordenadas c) (buildPois p)

buildCoordenadas :: String -> [Coordenada]
buildCoordenadas = map (buildCoordenadaFromStrings . splitCsvValues) . lines

buildCoordenadaFromStrings :: [String] -> Coordenada
buildCoordenadaFromStrings (x:s:a:t) = criaCoordenada (read x :: Double, read s :: Double) (read a :: Double) (readTime (head t))

buildPois :: String -> [Poi]
buildPois = map (buildPoiFromStrings . splitCsvValues) . lines

buildPoiFromStrings :: [String] -> Poi
buildPoiFromStrings (x:s:xs) = criaPoi (read x, read s) (head xs)
