module Gafipf.Percursos
( 
    Percurso,
    criaPercurso,
    toJson,
    getGanhoAltitude
) where

import Data.List (intercalate)
import Data.Fixed
import Gafipf.Poi
import Gafipf.Coordenadas

data Percurso = Percurso {
    coordenadas :: [Coordenada],
    sitios :: [String]
}

type Ponto = (Double, Double)

toJson :: Percurso -> String
toJson p = "{\n\"Categoria\": \"" ++ getCategoria p ++ "\",\n" ++ 
    "\"Tempo total (m)\":" ++ show (getTempo p) ++ ",\n\"Ganho acumulado\":" ++ 
    show (getGanho p) ++ ",\n\"Ganho acumulado por m\":" ++ 
    show (getGanhoMetro p) ++ ",\n\"Pontos de Interesse\": [\"" ++ 
    intercalate "\",\n\t\"" (sitios p) ++ "\"]\n}"

    
criaPercurso :: [Coordenada] -> [Poi] -> Percurso
criaPercurso coordenadas pois = Percurso coordenadas (getPoisInPercurso coordenadas pois)

getCategoria :: Percurso -> String
getCategoria p
    | (dist < 10000) && (alt < 500) = "A"
    | (dist < 12000) && (alt < 800) = "B"
    | (dist < 15000) && (alt < 1500) = "C"
    | otherwise = "D"
    where 
      dist = getDistancia p
      alt = getGanho p

getTempo :: Percurso -> Int
getTempo p = fromIntegral (getTime (last (coordenadas p)) - fromIntegral (getTime (head (coordenadas p)))) `div` 60

getGanho :: Percurso -> Int
getGanho p = round (getGanhoAltitude (map getAltitude (coordenadas p)))

getGanhoAltitude :: [Double] -> Double
getGanhoAltitude [] = 0
getGanhoAltitude [x] = 0
getGanhoAltitude [x,s] = if x < s then s-x else 0
getGanhoAltitude (x:s:xs) = (if x < s then s-x else 0) + getGanhoAltitude (s:xs)

getGanhoMetro :: Percurso -> Double
getGanhoMetro p = round3dp (getGanhoAltitude (map getAltitude (coordenadas p)) / getDistancia p)

round3dp :: Double -> Double
round3dp x = fromIntegral (round $ x * 1e3) / 1e3

getSitios :: Percurso -> [String]
getSitios p = map read (sitios p)

getDistancia :: Percurso -> Double
getDistancia p = 85000 * distancia (map getPoint (coordenadas p))

getPoisInPercurso :: [Coordenada] -> [Poi] -> [String]
getPoisInPercurso cs ps = [getLocal x | x <- ps, isPoiInPercurso cs x]

isPoiInPercurso :: [Coordenada] -> Poi -> Bool
isPoiInPercurso c p = 
    let ponto = getPonto p
    in foldl (\acc x -> inNeighbourhood ponto x || acc) False (getPoints c)

inNeighbourhood :: Ponto -> Ponto -> Bool
inNeighbourhood p1 p2 = getNumResolution (fst p1) > distancia2Pontos p1 p2

distancia2Pontos :: Ponto -> Ponto -> Double
distancia2Pontos (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

distancia :: [(Double, Double)] -> Double
distancia [] = 0
distancia [x] = 0
distancia (x:s:xs) = distancia2Pontos x s + distancia (s:xs)

-- Check if the number is integer
isInt :: (RealFrac a) => a -> Bool
isInt x = x == fromInteger (round x)

getNumResolution :: (RealFrac a) => a -> a
getNumResolution num = last [10^^x | x <- [(-10)..0], isInt (num / 10^^x)]