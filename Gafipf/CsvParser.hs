module Gafipf.CsvParser
  ( splitByChar
  , splitCsvValues
  , splitTime
  , getIntTime
  , readTime
  ) where

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
getIntTime (x:s:xs) = 60 * 60 * read x + (60 * read s) + read (head xs)

readTime :: String -> Int
readTime = getIntTime . splitTime
