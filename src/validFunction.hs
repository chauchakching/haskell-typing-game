-- Enter your code here. Read input from STDIN. Print output to STDOUT

import Control.Monad
import Data.List

f xys = all (\xiyi -> all (\xkyk -> xiyi == xkyk || fst xkyk /= fst xiyi) xys) xys

main = do
    t <- readLn :: IO Int
    forM_ [1..t] $ \_ -> do
        n <- readLn :: IO Int
        strPairs <- replicateM n getLine
        let pairs = map (\[x,y] -> (x,y)) $ map (map (read :: String -> Int)) $ map words strPairs
        putStrLn $ if f pairs
            then "YES"
            else "NO"