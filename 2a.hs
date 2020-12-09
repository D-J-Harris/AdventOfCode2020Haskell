import Prelude
import Text.Parsec hiding(count)

main = interact $ (++"\n") . show . f . rights . map (parse p "") . lines

p :: Parsec String () (Int, Int, Char, String)
p = do
  low <- many1 digit
  char '-'
  high <- many1 digit
  char ' '
  c <- letter
  string ": "
  s <- many1 letter
  return (read low, read high, c, s)

rights :: [Either a b] -> [b]
rights (Right x:xs) = x:rights xs
rights (_:xs) = rights xs
rights [] = []

count :: Eq a => a -> [a] -> Int
count c = length . filter (==c)

f :: [(Int, Int, Char, String)] -> Maybe Int
f xs = Just (count True $ map logic xs)
logic (low, high, c, s) = let n = count c s in  n >= low && n <= high
