module TestEngine (TestResult (..), be, resultOf, runSuite, should, shouldBe) where

import Control.Exception (ErrorCall, try, evaluate)
import Data.Functor ((<&>))

data TestResult a = Result a | Error deriving Eq

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
  let
    results = sequence $ map (\(u,v) -> tryResult (f u) <&> (== v)) tests
  in
    do passedTests <- (length . filter (== True)) <$> results
       failedTests <- (length . filter (== False)) <$> results

       putStrLn $ "Suite " ++ show name ++ ":"
       putStrLn $ "Tests: " ++ show passedTests ++ " passed, " ++ show failedTests ++ " failed"
       putStrLn ""
