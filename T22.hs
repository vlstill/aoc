{-# LANGUAGE LambdaCase, UnicodeSyntax, TypeApplications, ScopedTypeVariables, ViewPatterns, TupleSections #-}

module T22 where

import Utils
import Indexable

import Prelude.Unicode
import Data.List
import Control.Arrow

import Debug.Trace

play ∷ [Int] → [Int] → (Bool, [Int])
play [] ys = (False, ys)
play xs [] = (True,  xs)
play (x:xs) (y:ys)
  | x > y     = play (xs ++ [x, y]) ys
  | y > x     = play xs (ys ++ [y, x])
  | otherwise = error $ "play " <> show (xs, ys)

main ∷ IO ()
main = do
    (a, b) ← parse <$> getContents
    print . sum . map (uncurry (*)) . zip [1..] . reverse . snd $ play a b
  where
    parse ∷ String → ([Int], [Int])
    parse = lines
        >>> filter (≠ "")
        >>> splitsBy ("Player" `isPrefixOf`)
        >>> (\[[], a, b] → (map read a, map read b))
