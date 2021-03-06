import Prelude

main = interact $ (++"\n") . show . f' . map (read :: String -> Int) . lines

f' :: [Int] -> Maybe Int
f' (x:xs) = case f xs (2020-x) of
    Nothing -> f' xs
    Just y -> Just (y * x)
f' [] = Nothing

f :: [Int] -> Int -> Maybe Int
f (x:xs) n = if (n - x) `elem` xs then Just (x * (n - x)) else f xs n
f [] _ = Nothing
