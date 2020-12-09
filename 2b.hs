import Helper
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

f :: [(Int, Int, Char, String)] -> Maybe Int
f xs = Just (count True $ map logic xs)
logic (low, high, c, s) = let l1 = s !! (low - 1)
                              l2 = s !! (high - 1)
                          in (l1 == c) /= (l2 == c)
