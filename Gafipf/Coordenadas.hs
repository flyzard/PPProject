module Gafipf.Coordenadas
( 
    Coordenada,
    criaCoordenada,
    getPoints,
    getPoint,
    getAltitude,
    getTime
) where

import Gafipf.CsvParser
import Data.List (intercalate, splitAt)

type Ponto = (Double, Double)

data Coordenada = Coordenada {
    ponto :: Ponto,
    altitude :: Double,
    time :: Int
}

criaCoordenada :: Ponto -> Double -> Int -> Coordenada
criaCoordenada = Coordenada

getPoints :: [Coordenada] -> [Ponto]
getPoints = map ponto

getPoint :: Coordenada -> Ponto
getPoint = ponto

getAltitude :: Coordenada -> Double
getAltitude = altitude

getTime :: Coordenada -> Int
getTime = time
