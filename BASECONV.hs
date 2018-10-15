import Control.Monad (replicateM_)

convertBase :: Int -> Int -> [Int] -> [Int]
convertBase b1 b2 [n] = [n]
convertBase b1 b2 ns
    | b1 < b2 = convertToHigherBase b1 b2 ns
    | b1 > b2 = convertToLowerBase b1 b2 ns
    | otherwise = ns

convertToHigherBase :: Int -> Int -> [Int] -> [Int]
convertToHigherBase b1 b2 ns = map f (zip [0..] ns)
 where
  f =  



getInts :: IO [Int]
getInts = fmap ((map read) . words) getLine

getTestCase :: IO [Int]
getTestCase = do
  _ <- getLine
  getInts

solve :: Int -> Int -> IO ()
solve b1 b2 = 
  do
    t <- getTestCase
    r <- return (convertBase b1 b2 t)
    putStrLn $ show . length $ r
    putStrLn $ unwords . (map show) $ r
    return ()

main = do
  [t,b1,b2] <- getInts
  replicateM_ t (solve b1 b2)