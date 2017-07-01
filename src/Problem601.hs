module Problem601 where

import Data.Maybe


streak :: Int -> Int
streak n
  | n `mod` 2 == 0 = 1
  | otherwise = (snd . head . filter check $ zip [(n+2)..] [3..]) - 1
  where check (a, b) = a `mod` b /= 0

getP :: Int -> Int -> Int
getP s n = length . filter (==s) . map streak $ [2..n]

solve :: Int
solve = sum . map (\x -> getP x (4^x)) $ [1..31]



getpos :: Int -> [Int]
getpos n = map fst . filter (\x -> snd x == n) . zip [1..] $ map streak [2..(4^31)]

f :: Int -> Int
f 1 = 1
f n = lcm n $ f (n-1)



basen :: Int -> Int
basen n = fromMaybe 0 . lookup n . map (pair (id, f)) $ [1..32]

rulen :: Int -> [Int]
rulen n = map ((`div`(basen n)) . fst) . filter (\x -> snd x == n) . zip [1..] $ map streak [2..(4^31)]




pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f, g) = pair (f . fst, g . snd)

