module Helper where
import Prelude

rights :: [Either a b] -> [b]
rights (Right x:xs) = x:rights xs
rights (_:xs) = rights xs
rights [] = []

count :: Eq a => a -> [a] -> Int
count c = length . filter (==c)