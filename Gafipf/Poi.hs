module Gafipf.Poi
( 
    Poi,
    criaPoi,
    getPonto,
    getLocal
) where

type Ponto = (Double, Double)
data Poi = Poi {
    ponto :: Ponto,
    local :: String
}

criaPoi :: Ponto -> String -> Poi
criaPoi = Poi

getPonto :: Poi -> Ponto
getPonto = ponto

getLocal :: Poi -> String
getLocal = local