-- Enter your code here. Read input from STDIN. Print output to STDOUT

import Data.List
import qualified Data.Graph as Graph

type Pairs = [[Int]]

f :: Int -> Pairs -> Int
f n pairs = sum $ map (ceiling . (**0.5) . fromIntegral . length) $ Graph.components graph
  where graph = Graph.buildG (1,n) $ map (\[a,b] -> (a,b)) pairs

readInt = read :: String -> Int

main = do
    n <- readLn :: IO Int
    m <- readLn :: IO Int
    contents <- getContents
    let pairs = map ((map readInt) . words) $ lines contents
    putStrLn $ show $ f n pairs
