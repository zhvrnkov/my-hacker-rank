import Data.List

main :: IO ()
main = interact $ staircase . read . head . words

staircase :: Int -> String
staircase n = intercalate "\n" . map act $ [1..n]
  where act = (\x -> stairline (n - x) x)

stairline :: Int -> Int -> String
stairline 0 0 = ""
stairline 0 n = '#': stairline 0 (n - 1)
stairline s n = ' ': stairline (s - 1) n