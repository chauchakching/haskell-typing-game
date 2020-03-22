import Text.Printf (printf)

dx = 0.001 :: Double

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = [sum $ map dA xs, sum $ map dV xs]
    where xs = drop 1 [fromIntegral l, (fromIntegral l) + dx .. fromIntegral r]
          dA x = (poly a b x) * dx
          dV x = disk_Vol (poly a b x) dx
          
poly a b x = sum $ zipWith (\ai bi -> (fromIntegral ai) * x**(fromIntegral bi)) a b

disk_Vol r h = pi * r^2 * h

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines