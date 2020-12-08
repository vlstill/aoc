{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables, LambdaCase #-}

module Utils where

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

count ∷ ∀α τ ι. (Foldable τ, Integral ι) ⇒ (α → Bool) → τ α → ι
count f = getSum . foldMap (Sum . fromIntegral . fromEnum . f)

htmap ∷ ∀α β. (α → β) → (α → β) → [α] → [β]
htmap hf tf (x:xs) = hf x : map tf xs
htmap _ _ [] = []

embrace ∷ String → String
embrace xs = "(" ++ xs ++ ")"
