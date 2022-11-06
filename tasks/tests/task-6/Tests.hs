module Tests where

import Task6 (unique, primesUpToN)
import Tests.Data (primeData, unionData)
import TestEngine

prop_UnionIsTotal xs = all elemOfUnion xs
  where us = unique xs
        elemOfUnion x = elem x us

prop_UnionContainsUniques xs = length repetitions == length us
  where us = unique xs
        repetitions = filter (==True) $ (==) <$> us <*> us

prop_PrimesArePrime n = all isPrime $ primesUpToN n
  where isPrime x = all ((/=0) . rem x) [2..x-1]

prop_PrimesUpToN n = all (<= n) $ primesUpToN n

prop_PrimesAreOrdered = isSorted . primesUpToN where
  isSorted []       = True
  isSorted [x]      = True
  isSorted (x:y:xs) = x <= y && isSorted (y:xs)


test_All =
  checkProperty "prop_PrimesArePrime" primeData prop_PrimesArePrime
  >> checkProperty "prop_PrimesUpToN" primeData prop_PrimesUpToN
  >> checkProperty "prop_PrimesAreOrdered" primeData prop_PrimesAreOrdered
  >> checkProperty "prop_UnionIsTotal" unionData prop_UnionIsTotal
  >> checkProperty "prop_UnionContainsUniques" unionData prop_UnionContainsUniques
