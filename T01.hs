{-# LANGUAGE UnicodeSyntax, ExplicitForAll, TupleSections, ScopedTypeVariables, ViewPatterns #-}

module T01a where

import Prelude ( Num (..), Integral (..), Eq, Ord ( (<), (>) ),
                 IO, Maybe (..),
                 ($), (<$>), (.), otherwise, undefined, read, uncurry,
                 getContents, print )
import Prelude.Unicode
import Data.Maybe
import Data.Vector ( fromList, Vector, (!), take, drop )
import Data.List ( sort, lines )
import Data.Foldable ( foldMap', length )
import Data.Monoid ( First ( First ), getFirst )

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

sums2To ∷ ∀α. (Num α, Ord α) ⇒ α → Vector α → Maybe (α, α)
sums2To n xs = getFirst $ foldMap' look xs
  where
    look ∷ α → First (α, α)
    look a = (a,) <$> First (binSearch xs (n - a))

sums3To ∷ ∀α. (Num α, Eq α) ⇒ α → Vector α → Maybe (α, α)
sums3To = undefined

main ∷ IO ()
main = do
    input <- fromList . sort . (read <$>) . lines <$> getContents
    print . uncurry (*) . fromJust . sums2To 2020 $ input
    print . uncurry (*) . fromJust . sums3To 2020 $ input
