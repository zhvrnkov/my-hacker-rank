main :: IO ()
main = do
  input <- getLine
  putStrLn $ process . foo . map read . words $ input

process :: (Real a, Show a) => [[a]] -> String
process result = (\[min, max] -> min ++ " " ++ max) $ [show . minimum, show . maximum] >>= (\f -> return $ f res)
  where res = map sum result

foo :: [a] -> [[a]]
foo list = map act [1..(length list)]
  where act = \i -> let (f, s) = splitAt i list
                    in init f ++ s