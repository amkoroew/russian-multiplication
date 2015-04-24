mul :: Int -> Int -> Int
mul x y = mul' x y [(x, y)]

mul' :: Int -> Int -> [(Int, Int)] -> Int
mul' 1 y s = sum [b | (a , b) <- s, a `mod` 2 /= 0]
mul' x y s = mul' x' y' ((x', y') : s)
   where x' = x `div` 2
         y' = y * 2
