{-# LANGUAGE UnicodeSyntax #-}

module T05 where

import Utils
import Data.List
import Data.Monoid

main ∷ IO ()
main = do
    groups ← splitsBy (== "") . lines <$> getContents
    print . lengthsSum $ foldr1 union <$> groups
    print . lengthsSum $ foldr1 intersect <$> groups
  where
    lengthsSum = getSum . foldMap (Sum . length)
