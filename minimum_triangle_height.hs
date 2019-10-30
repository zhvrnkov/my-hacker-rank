main = interact $ show . (\[b, a] -> minimum_height b a) . map read . words

minimum_height :: (RealFrac a, Integral b) => a -> a -> b
minimum_height b a = ceiling . (2 *) $ a / b