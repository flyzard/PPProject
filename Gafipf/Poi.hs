module Gafipf.Poi
( 
    Poi,
    criaPois
) where

type Ponto = (Double, Double)
data Poi = Poi {
    point :: Ponto,
    poi :: String
}

criaPois :: Double -> Double -> String -> Poi
criaPois lat longe poi = Poi lat longe poi