module Gafipf.CsvParser (
    splitByChar,
    splitCsvValues,
    splitTime,
    getIntTime,
    readTime
) where

-- parseCsv :: CsvFileType -> String -> [[String]]
-- parseCsv t
--     | t == Coordenadas = buildCoordenadas . f
--     | t == Pois = buildPois . f
--     | otherwise = []
--     where f = map splitCsvValues . lines

-- ponto :: Ponto,
-- altitude :: Double,
-- time :: TimeOfDay


-- buildCoordenadas :: [String] -> Coordenada
-- buildCoordenadas s = 
-- buildPois

-- percurso <- readFile "resources/BairrosAntigosLisboa.csv"
-- pois <- readFile "resources/LisboaPOI.csv"
-- ls = lines percurso
-- map splitCsvValues ls

    
splitByChar :: Char -> String -> [String]
splitByChar c [x] = [[x]]
splitByChar c (x:xs)
    | x == c = "" : rest
    | otherwise = (x : head rest) : tail rest
    where
        rest = splitByChar c xs

splitCsvValues :: String -> [String]
splitCsvValues = splitByChar ','

splitTime :: String -> [String]
splitTime = splitByChar ':'

getIntTime :: [String] -> Int
getIntTime (x:s:xs)  = 60 * 60 * read x + (60 * read s) + read (head xs)

readTime :: String -> Int
readTime = getIntTime . splitTime
