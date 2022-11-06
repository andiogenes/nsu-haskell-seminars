module Task6 (unique, primesUpToN) where

newtype Union a = Union { getUnion :: [a] } deriving (Eq, Show)

instance Eq a => Semigroup (Union a) where
  (<>) = undefined

instance (Eq a) => Monoid (Union a) where
  mempty = Union []
  mappend = (<>)

unique :: Eq a => [a] -> [a]
unique = undefined
  where
    singleton x = [x]
    listOfUnion = fmap (Union . singleton)

primesUpToN :: Int -> [Int]
primesUpToN = undefined
