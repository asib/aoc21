module Day1 where

part1 :: String -> Integer
part1 = part1' 0 . map read . lines

part1' :: Integer -> [Integer] -> Integer
part1' n [] = n
part1' n [_] = n
part1' n (a:b:bs)
    | b > a = part1' (n+1) (b:bs)
    | otherwise = part1' n (b:bs)

part2 :: [Integer] -> Integer 
part2 xs = part1' 0 $ zipWith3 (\a b c -> a + b + c) xs (drop 1 xs) (drop 2 xs)