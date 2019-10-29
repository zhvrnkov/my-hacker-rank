import Data.List

main = interact process

process :: String -> String
process = showPoints . map calc . splitBy 2 . parse . map read . tail. words
  where calc = \[f, s] -> nextOnLine f s

parse :: (Num a) => [a] -> [Point a]
parse nums = map (\[x, y] -> Point x y) $ splitBy 2 nums

showPoints :: (Show a) => [Point a] -> String
showPoints points = intercalate "\n" $ map show points

splitBy :: Int -> [b] -> [[b]]
splitBy _ [] = []
splitBy 0 list = [list]
splitBy factor list = f: splitBy factor s
  where (f,s) = splitAt factor list

data Point a = Point a a

instance (Show a) => Show (Point a) where
  show (Point x y) = show x ++ " " ++ show y

nextOnLine :: Num a => Point a -> Point a -> Point a
nextOnLine (Point px py) (Point qx qy) = Point x y
  where x = 2 * qx - px
        y = 2 * qy - py