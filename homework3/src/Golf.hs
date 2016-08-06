{-# OPTIONS_GHC -Wall #-}
module Golf where

-- Exercise 1
skip :: Int -> [a] -> [a]
skip n zs = map snd (filter (\(i,_) -> i `mod` n == 0 ) (zip [1..] zs))

skips :: [a] -> [[a]]
skips zs = zipWith skip [1 ..] $ replicate (length zs) zs

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | y>x && y>z = y : localMaxima (y:z:zs)
  | otherwise  = localMaxima (y:z:zs)
localMaxima _  = []

-- Exercise 3
histogram :: [Integer] -> String
histogram xs = let occ = occurrences xs
                   result = map (`line` occ) [1..maximum occ] in
                   unlines (result ++ histogramSeparator : [histogramRangeString])

line :: Int -> [Int] -> String
line n = map (\i -> if i >= n then '*' else ' ')

histogramRange :: [Integer]
histogramRange = [0..9]

histogramRangeString :: String
histogramRangeString = foldl (\acc i -> acc ++ show i) "" histogramRange

histogramSeparator :: String
histogramSeparator = map (const '=') histogramRange

occurrences :: [Integer] -> [Int]
occurrences xs = map (\i -> length $ filter (==i) xs) histogramRange
