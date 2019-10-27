import Data.List

main = interact $ show . getStats . tail . map read . words

getStats :: (Real a, Num b) => [a] -> Stats b
getStats list = foldl act (Stats 0 0 0) list
  where act = \acc x -> case () of
                           _ | x > 0 -> incPositives acc
                             | x < 0 -> incNegatives acc
                             | otherwise -> incZeroes acc

data Stats a = Stats { positives :: a, negatives :: a, zeroes :: a }

instance (Fractional a, Show a) => Show (Stats a) where
  show (Stats p n z) = show (p / t) ++ newLine ++ show (n / t) ++ newLine ++ show (z / t)
    where t = p + n + z
          newLine = "\n"

incPositives :: (Num a) => Stats a -> Stats a
incPositives (Stats p n z) = Stats (p + 1) n z

incNegatives :: (Num a) => Stats a -> Stats a
incNegatives (Stats p n z) = Stats p (n + 1) z

incZeroes :: (Num a) => Stats a -> Stats a
incZeroes (Stats p n z) = Stats p n (z + 1)
