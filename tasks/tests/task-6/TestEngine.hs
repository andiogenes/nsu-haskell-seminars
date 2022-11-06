module TestEngine (TestResult (..), be, checkProperty, resultOf, runSuite, should, shouldBe) where

import Control.Exception (ErrorCall, try, evaluate)
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.List (partition, find)

data TestResult a = Result a | Error deriving (Eq, Show)

resultOf = id

be = id

should = (,)

shouldBe u v = (u, Result v)

tryResult :: a -> IO (TestResult a)
tryResult v = (try $ evaluate v) <&> (either produceError Result)
  where produceError :: ErrorCall -> TestResult a
        produceError = const Error

runSuite :: Eq b => String -> [(a, TestResult b)] -> (a -> b) -> IO ()
runSuite name tests f =
  do let results = sequence $ map (\(u,v) -> tryResult (f u) <&> (== v)) tests
     (passedTests, failedTests) <- (bimap length length . partition (== True)) <$> results

     putStrLn $ "Suite " ++ show name ++ ":"
     putStrLn $ "Tests: " ++ show passedTests ++ " passed, " ++ show failedTests ++ " failed"
     putStrLn ""

defaultSampleSize = 100

checkProperty :: Show a => String -> (Int -> [IO a]) -> (a -> Bool) -> IO ()
checkProperty name gen prop =
  do samples <- sequence $ gen defaultSampleSize
     let runs = zip samples $ fmap prop samples
     let fail = find (\(_, result) -> result == False) runs
     do case fail of
          Nothing -> putStrLn $ mconcat ["Test ", name, " succeed."]
          Just (inp, _) -> putStrLn $ mconcat ["Test ", name, " failed. Counterexample: ", show inp]
     return ()
