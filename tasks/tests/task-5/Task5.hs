module Task5 where

import Tests

toSeries :: String -> [String]
toSeries = undefined

encodeSeries :: [String] -> [(Int, Char)]
encodeSeries = undefined

writeCode :: [(Int, Char)] -> String
writeCode = undefined

makeAssoc :: Eq a => [(a, b)] -> a -> b
makeAssoc = undefined
  where
    emptyAssoc = error "no such key"
    insert k v find = \x -> if x == k then v else find x

 
test_All =
  test_toSeries toSeries
  >> test_encodeSeries encodeSeries 
  >> test_writeCode writeCode
  >> test_makeAssoc makeAssoc

