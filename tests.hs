import Test.QuickCheck
import Data.List

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

prop_idempotent :: [Integer] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

isEven x = x `mod` 2 == 0

evenAfterOdd :: [Integer] -> Bool
evenAfterOdd [] = True
evenAfterOdd [x] = True
evenAfterOdd (x:s:xs) = ((not (isEven x) && isEven s) || (not (isEven s) && isEven x)) && evenAfterOdd (s : xs)

prop_evenAfterOdd :: Integer -> Bool
prop_evenAfterOdd x = (isEven x && not (isEven (x + 1))) || (not (isEven x) && isEven (x + 1))

runTest = verboseCheck prop_evenAfterOdd