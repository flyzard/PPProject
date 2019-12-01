module Gafipf.Percursos
( 
    Percurso,
    criaPercurso,
    toJson
) where

import Data.List (intercalate, splitAt)
import Data.Time
import Gafipf.Poi
import Gafipf.Coordenadas

data Percurso = Percurso {
    coordenadas :: [Coordenada],
    sitios :: [String]
}

criaPercurso :: [Coordenada] -> [String] -> Percurso
criaPercurso coordenadas sitios = Percurso coordenadas sitios

toJson :: Percurso -> String
toJson [] = ""
toJson p = "{\"Categoria\": \"" ++ getCategoria p ++ "\"\n" ++ 
    "\"Tempo total (m)\":" ++ show (getTempo p) ++ "\n\"Ganho acumulado\":" ++ 
    show (getGanho p) ++ "\n\"Ganho acumulado por m\":" ++ 
    show (getGanhoMetro p) ++ "\n\"Pontos de Interesse\": [\"" ++ 
    (intercalate "\", \" " sitios) ++ "\n\"]}"

getCategoria :: Percurso -> Char
getCategoria p
    | (dist < 10000) && (alt < 500) = "A"
    | (dist < 12000) && (alt < 800) = "B"
    | (dist < 15000) && (alt < 1500) = "C"
    | otherwise = "D"
    where dist = getDistancia p
        alt = getGanho p

getTempo :: Percurso -> Int
getTempo p = DiffTime (time head p) (time last p)

getGanho :: Percurso -> Int
getGanho p = foldl (\acc (x:s:xs) -> acc + if x < s then x-s else 0) 0 (map altitude p)

getGanhoMetro :: Percurso -> Float
getGanhoMetro

getPontos :: Percurso -> [String]
getPontos p = sitios p

getDistancia :: Percurso -> Double
getDistancia = distancia . map ponto . map coordenadas

buildPercurso :: [Coordenadas] -> [Poi] -> Percurso
buildPercurso c p = map (\x -> criaVisita )

navisinhanca :: Coordenada -> Poi -> Bool
navisinhanca c p = 

distancia2Pontos :: Ponto -> Ponto -> Double
distancia2Pontos (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

distancia :: (Ord a, Double a) => [(a, a)] -> a
distancia [] = 0
distancia (x:[]) = 0
distancia (x:s:xs) = (distancia2Pontos x s) + distancia (s:xs)



-- check which points are on the coordenadas and store point name, 
 