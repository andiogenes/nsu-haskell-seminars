import Data.Monoid (Sum (..))

faculty :: String -> String
faculty name = ((name ++ ": ") ++) . task . hash $ name
  where
    task n | n == 1 = "Красное задание!"
           | otherwise = "Зеленое задание!"
    hash = (`mod` 2) . getSum . mconcat . fmap (Sum . (`mod` 17) . fromEnum)

main :: IO ()
main = getContents >>= mapM_ (putStrLn . faculty) . lines
