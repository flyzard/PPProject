module Gafipf.Poi
( 
    Poi,
    criaPoi,
    getPonto,
    getLocal
) where

import Gafipf.Coordenadas (Ponto)

data Poi = Poi {
    ponto :: Ponto,
    local :: String
}

instance Show Poi where
    show (Poi p l) = "(" ++ show (fst p) ++ ", " ++ show (snd p) ++ ") -> " ++ l ++ "\n"

criaPoi :: Ponto -> String -> Poi
criaPoi = Poi

getPonto :: Poi -> Ponto
getPonto = ponto

getLocal :: Poi -> String
getLocal = local