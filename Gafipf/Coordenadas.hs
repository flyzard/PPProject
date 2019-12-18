module Gafipf.Coordenadas
( Coordenada,
    criaCoordenada,
    getPoints,
    getPoint,
    getAltitude,
    getTime,
    Ponto
) where

type Ponto = (Double, Double)

data Coordenada = Coordenada {
    ponto :: Ponto,
    altitude :: Double,
    time :: Int
}

instance Show Coordenada where
    show (Coordenada p a t) = "(" ++ show (fst p) ++ ", " ++ show (snd p) ++ ") -> "
        ++ "Altitude: " ++ show a ++ " -> " ++ "time: " ++ show t  ++ "\n"

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
