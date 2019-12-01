module Gafipf.Coordenadas
( 
    Coordenada,
    criaCoordenadas
) where

import Data.Time

type Ponto = (Double, Double)

data Coordenada = Coordenada {
    ponto :: Ponto,
    altitude :: Double,
    time :: TimeOfDay
}

criaCoordenadas :: Double -> Double -> Double -> TimeOfDay -> Coordenada
criaCoordenadas lat long alt time = Coordenada lat long alt time