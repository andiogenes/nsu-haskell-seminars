module Tests (test_toSeries, test_encodeSeries, test_writeCode, test_makeAssoc) where

import TestEngine

test_toSeries :: (String -> [String]) -> IO ()
test_toSeries = runSuite "test_toSeries"
  [
    resultOf
      "abbbccccbbba"
    `shouldBe` 
      ["a","bbb","cccc","bbb","a"]
  ]

test_encodeSeries :: ([String] -> [(Int, Char)]) -> IO ()
test_encodeSeries = runSuite "test_encodeSeries"
  [
    resultOf
      ["a","bbb","cccc","bbb","a"]
    `shouldBe`
      [(1,'a'),(3,'b'),(4,'c'),(3,'b'),(1,'a')]
  ]


test_writeCode :: ([(Int, Char)] -> String) -> IO ()
test_writeCode = runSuite "test_writeCode"
  [
    resultOf
      [(1,'a'),(3,'b'),(4,'c'),(3,'b'),(1,'a')]
    `shouldBe`
      "1a3b4c3b1a"
  ]

test_makeAssoc makeAssoc = runSuite "test_makeAssoc"
  [ resultOf 2 `shouldBe` "two"
  , resultOf 1 `shouldBe` "one"
  , resultOf 3 `shouldBe` "three"
  , resultOf 4 `should` be Error
  ] (makeAssoc [(1, "one"), (2, "two"), (3, "three"), (1, "ten")])
