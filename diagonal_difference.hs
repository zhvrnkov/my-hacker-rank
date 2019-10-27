import Data.Monoid

main = interact process

process :: String -> String
process = show . diagonalDifference . integralListToSquareMatrix . map read . words

data SquareMatrix a b = SquareMatrix a [[b]] deriving (Show)

integralListToSquareMatrix :: (Integral a) => [a] -> SquareMatrix a a 
integralListToSquareMatrix list = getSquareMatrix (head list) (tail list)

getSquareMatrix :: (Integral a ,Integral b) => a -> [b] -> SquareMatrix a b
getSquareMatrix length list = SquareMatrix length $ splitIntoRows length list

diagonalDifference :: (Integral a, Integral b) => SquareMatrix a b -> b
diagonalDifference matrix = abs $ (head pair) - (last pair)
  where pair = [leftDiagonal, rightDiagonal] >>= (\f -> return $ f matrix)

leftDiagonal :: (Integral a, Integral b) => SquareMatrix a b -> b
leftDiagonal (SquareMatrix length things) = sum . map get $ indices
  where indices = [0..(fromIntegral length - 1)]
        get = (\i -> (things !! i) !! i)

rightDiagonal :: (Integral a, Integral b) => SquareMatrix a b -> b
rightDiagonal (SquareMatrix length things) = sum . map get $ indices
  where plainIndices = [0..(fromIntegral length - 1)]
        indices = zip plainIndices $ reverse plainIndices
        get = (\(c, r) -> (things !! r) !! c)

splitIntoRows :: Integral a => a -> [b] -> [[b]]

splitIntoRows lengthOfRow list = foldr action [] list
  where headIsReady = \(x:xs) -> (== fromIntegral lengthOfRow) $ length x
        conditions = [null, headIsReady]
        predicate = anyReturnTrue conditions
        action = (\x acc -> if predicate acc
                            then [x] : acc
                            else (x: (head acc)): tail acc)

anyTrue = getAny . mconcat . map Any

anyReturnTrue :: [a -> Bool] -> a -> Bool
anyReturnTrue conditions thing =
  anyTrue $ conditions >>= (\f -> return $ f thing)

