main :: IO ()
main = do
     aInput <- getLine
     bInput <- getLine
     putStrLn $ processInput aInput bInput

processInput :: String -> String -> String
processInput a b = pairToString . getScore . map subtractPair $ zip aNums bNums
  where aNums = map read . words $ a
        bNums = map read . words $ b
        pairToString = (\(a, b) -> show a ++ " " ++ show b)

getScore :: Real a => [a] -> (Int, Int)
getScore state = foldl act (0, 0) state
  where act = (\acc x -> case () of
                            _ | x > 0 -> ((+1) . fst $ acc, snd acc)
                              | x < 0 -> (fst acc, (+1) . snd $ acc)
                              | otherwise -> acc)

subtractPair :: Num a => (a, a) -> a
subtractPair (x, y) = x - y