{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables, LambdaCase, ViewPatterns #-}

module Utils where

import Prelude.Unicode
import Data.Monoid ( Sum ( Sum ), getSum )
import Data.List ( unfoldr )
import Control.Arrow ( second )

splitsBy ∷ ∀α. (α → Bool) → [α] → [[α]]
splitsBy p = unfoldr (\case
                        [] → Nothing
                        xs → Just . second (drop 1) $ break p xs)

ltrim, rtrim, trim ∷ ∀α. (α → Bool) → [α] → [α]
ltrim = dropWhile
rtrim f = reverse . ltrim f . reverse
trim f = rtrim f . ltrim f

count ∷ ∀ι α τ. (Foldable τ, Integral ι) ⇒ (α → Bool) → τ α → ι
count f = getSum . foldMap (Sum . fromIntegral . fromEnum . f)

htmap ∷ ∀α β. (α → β) → (α → β) → [α] → [β]
htmap hf tf (x:xs) = hf x : map tf xs
htmap _ _ [] = []

embrace ∷ String → String
embrace xs = "(" ++ xs ++ ")"

mayToList ∷ ∀α. Maybe α → [α]
mayToList (Just x) = [x]
mayToList Nothing  = []

fixpt ∷ ∀α. Eq α ⇒ (α → α) → α → α
fixpt f = fst . fixptn f

fixptn ∷ ∀α. Eq α ⇒ (α → α) → α → (α, Int)
fixptn f = go 1
  where
    go ∷ Int → α → (α, Int)
    go n x@(f → x1)
      | x ≡ x1    = (x, n)
      | otherwise = go (n + 1) x1
