-- Enter your code here. Read input from STDIN. Print output to STDOUT

shoelace :: Floating a => [(a,a)] -> a
shoelace pts = (*0.5) . abs . sum $ zipWith op pts (last pts : pts)
    where op (x1, y1) (x2, y2) = x1*y2 - x2*y1

readFloat = read :: String -> Float

main = do
    n <- readLn :: IO Int
    contents <- getContents
    let pts = map ((\[x,y] -> (readFloat x, readFloat y)) . words) $ lines contents
    putStrLn $ show $ shoelace pts
