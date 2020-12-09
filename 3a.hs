import Helper
import Prelude

main = interact $ (++"\n") . show . exec . map (map (=='#')) . lines

exec :: [[Bool]] -> Maybe Int
exec x = Just $ count True $ f x (3, 0)

-- could also use cycle . map to extend the map, instead of modulo
f :: [[Bool]] -> (Int, Int) -> [Bool]
f (p:ps) (xv, x) = (p !! (x `mod` length p)) : f ps (xv, x+xv)
f _ (_, _) =  []
