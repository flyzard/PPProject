module Main where

import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
import Gafipf.Percursos
import Gafipf.CsvParser
import Gafipf.Coordenadas
import Gafipf.Poi
import Test.QuickCheck

main :: IO ()
main = toTry `catch` handler

toTry :: IO ()
toTry = do args <- getArgs
           controller args

controller :: [String] -> IO ()
controller [] = error "Indique os argumentos do programa!"
controller [x] = if x == "-t" then runTests else error "Argumento inválido!"
controller [r, p, f] = do percurso <- readFile r
                          pontos <- readFile p 
                          buildJson percurso pontos f
controller xs = error "Numero de argumentos invalido!"

runTests :: IO ()
runTests = return () --do quickCheck prop_

handler :: IOError -> IO ()     
handler e  
    | isDoesNotExistError e =
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path  
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"  
    | otherwise = ioError e   

--Builds a json file
buildJson :: String -> String -> String -> IO ()
buildJson percurso pontos jsonPath = writeFile jsonPath (toJson (buildPercurso percurso pontos))

--Builds the route from all the processed information, ready to be submitted to the GAFIPF website
buildPercurso :: String -> String -> Percurso
buildPercurso c p = criaPercurso (buildCoordenadas c) (buildPois p)

--Separates strings by line (\n), then separates them by comma (,) and proceeds to create coordinates with the latitude, longitude, altitude and time
buildCoordenadas :: String -> [Coordenada]
buildCoordenadas = map (buildCoordenadaFromStrings . splitCsvValues) . lines

--Returns the coordinates from a string
buildCoordenadaFromStrings :: [String] -> Coordenada
buildCoordenadaFromStrings (x:s:a:t) = criaCoordenada (read x :: Double, read s :: Double) (read a :: Double) (readTime (head t))

--Separates strings by line (\n), then separates them by comma (,) and proceeds to create points of interest from their latitude, longitude and name
buildPois :: String -> [Poi]
buildPois = map (buildPoiFromStrings . splitCsvValues) . lines

--Returns the point of interest from a string
buildPoiFromStrings :: [String] -> Poi
buildPoiFromStrings (x:s:xs) = criaPoi (read x, read s) (head xs)
