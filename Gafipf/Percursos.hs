module Gafipf.Percursos
( 
    Percurso,
    criaPercurso,
    toJson,
    getGanhoAltitude
) where

import Gafipf.Poi
import Gafipf.Coordenadas
import Data.List (intercalate)
import Numeric

data Percurso = Percurso {
    coordenadas :: [Coordenada],
    sitios :: [String]
}

--Creates a template of json file
toJson :: Percurso -> String
toJson p = "{\n\"Categoria\": \"" ++ getCategoria p ++ "\",\n" ++ 
    "\"Tempo total (m)\":" ++ show (getTempo p) ++ ",\n\"Ganho acumulado\":" ++ 
    show (getGanho p) ++ ",\n\"Ganho acumulado por m\":" ++ 
    showDecimal (ganhoAcumulado p) ++ ",\n\"Pontos de Interesse\": [\"" ++ 
    intercalate "\",\n\t\"" (sitios p) ++ "\"]\n}"

 --Creates a route   
criaPercurso :: [Coordenada] -> [Poi] -> Percurso
criaPercurso coordenadas pois = Percurso coordenadas (getPoisInPercurso coordenadas pois)

--Hiking difficulty
getCategoria :: Percurso -> String
getCategoria p
    | (dist < 10000) && (alt < 500) = "A"
    | (dist < 12000) && (alt < 800) = "B"
    | (dist < 15000) && (alt < 1500) = "C"
    | otherwise = "D"
    where 
      dist = getDistancia p
      alt = getGanho p

--The amount of time it takes to hike the route
getTempo :: Percurso -> Int
getTempo p = fromIntegral (getTime (last $ coordenadas p) - getTime (head $ coordenadas p)) `div` 60

--How much you'll climb in the hike
getGanho :: Percurso -> Int
getGanho p = round (getGanhoAltitude (map getAltitude (coordenadas p)))

--Returns the gain in height between multiple heights
getGanhoAltitude :: [Double] -> Double
getGanhoAltitude [] = 0
getGanhoAltitude [x] = 0
getGanhoAltitude [x,s] = if x < s then s-x else 0
getGanhoAltitude (x:s:xs) = (if x < s then s-x else 0) + getGanhoAltitude (s:xs)

--How much you'll climb for each metre you walk horizontally
ganhoAcumulado :: Percurso -> Double
ganhoAcumulado p = round3dp (getGanhoAltitude (map getAltitude (coordenadas p)) / getDistancia p)

--Rounds numbers to their 3 decimal place
round3dp :: Double -> Double
round3dp x = fromIntegral (round $ x * 1e3) / 1e3

-- Necessario para representar o numero em decimal, em vez de notação cientifica
showDecimal :: Double -> String
showDecimal x = showFFloat Nothing x ""

--Returns the names of the places visited
getSitios :: Percurso -> [String]
getSitios p = map read (sitios p)

--Returns the names of the places visited
getDistancia :: Percurso -> Double
getDistancia p = 85000 * distancia (map getPoint (coordenadas p))

--Points of interest from the route
getPoisInPercurso :: [Coordenada] -> [Poi] -> [String]
getPoisInPercurso cs ps = [getLocal x | x <- ps, isPoiInPercurso cs x]

--Checks if a certain point of interest is visited in the route
isPoiInPercurso :: [Coordenada] -> Poi -> Bool
isPoiInPercurso c p = 
    let ponto = getPonto p
    in foldl (\acc x -> inNeighbourhood ponto x || acc) False (getPoints c)

--Checks if a certain place is visited or if the hike just goes through it's neighbourhood
inNeighbourhood :: Ponto -> Ponto -> Bool
inNeighbourhood p1 p2 = getNumResolution (fst p1) > distancia2Pontos p1 p2

--Returns the distance between 2 points
distancia2Pontos :: Ponto -> Ponto -> Double
distancia2Pontos (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

--Returns the distance between multiple points
distancia :: [(Double, Double)] -> Double
distancia [] = 0
distancia [x] = 0
distancia (x:s:xs) = distancia2Pontos x s + distancia (s:xs)

-- Check if the number is integer
isInt :: (RealFrac a) => a -> Bool
isInt x = x == fromInteger (round x)

--Returns the number resolution
getNumResolution :: (RealFrac a) => a -> a
getNumResolution num = last [10^^x | x <- [(-10)..0], isInt (num / 10^^x)]