{-# LANGUAGE UnicodeSyntax, TupleSections, ScopedTypeVariables, ViewPatterns, TypeApplications #-}

module T01 where

import Prelude ( Num (..), Integral (..), Ord ( (<), (>) ), IO, Integer,
                 ($), (<$>), (.), otherwise, read, uncurry,
                 getContents, print )
import Prelude.Unicode ( (≡), (≤) )
import Data.Maybe ( Maybe (..), fromJust )
import Data.Vector ( fromList, Vector, (!), take, drop )
import Data.List ( sort, lines )
import Data.Foldable ( foldMap', length )
import Data.Monoid ( Alt ( Alt ), getAlt )

binSearch ∷ ∀α. Ord α ⇒ Vector α → α → Maybe α
binSearch vec n
  | halfVal ≡ n = Just halfVal
  | l ≤ 1       = Nothing
  | n < halfVal = binSearch (take half vec) n
  | otherwise   = binSearch (drop (half + 1) vec) n
  where
    l = length vec
    half = l `div` 2
    halfVal = vec ! half

-- O(n log n)
sums2To ∷ ∀α. (Num α, Ord α) ⇒ α → Vector α → Maybe (α, α)
sums2To n xs = getAlt $ foldMap' look xs
  where
    look ∷ α → Alt Maybe (α, α)
    look a = (a,) <$> Alt (binSearch xs (n - a))

-- O(n²)
sums3To ∷ ∀α. (Num α, Ord α) ⇒ α → Vector α → Maybe (α, α, α)
sums3To n xs@(length → l) = go 0 1 (l - 1)
  where
    go ia@((xs !) → a) ib@((xs !) → b) ic@((xs !) → c)
      | ib ≡ ic   = go (ia + 1) (ia + 2) (l - 1)
      | abc ≡ n   = Just (a, b, c)
      | abc > n   = go ia ib (ic - 1)
      | otherwise = go ia (ib + 1) ic
      where
        abc = a + b + c


main ∷ IO ()
main = do
    input <- fromList . sort @Integer . (read <$>) . lines <$> getContents
    print . uncurry (*) . fromJust . sums2To 2020 $ input
    print . (\(a, b, c) -> a * b * c) . fromJust . sums3To 2020 $ input
