import Prelude

main = interact $ (++"\n") . show . f . map (read :: String -> Int) . lines

f :: [Int] -> Maybe Int
f (x:xs) = if (2020 - x) `elem` xs then Just (x * (2020 - x)) else f xs
f [] = Nothing
