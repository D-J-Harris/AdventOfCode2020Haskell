import Helper
import Prelude

main = interact $ (++"\n") . show . exec . map (map (=='#')) . lines

exec :: [[Bool]] -> Maybe Int
exec m = Just $ product $ map (f' m) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

f' m (xv, yv) = count True $ f m (xv, yv) (0, 0+1)

-- could also use cycle . map to extend the map, instead of modulo
f :: [[Bool]] -> (Int, Int) -> (Int, Int) -> [Bool]
f (p:ps) (xv, yv) (x, 1) = (p !! (x `mod` length p)) : f ps (xv, yv) (x+xv, yv)
f (_:ps) (xv, yv) (x, y) = f ps (xv, yv) (x, y-1)
f _ _ _ =  []
