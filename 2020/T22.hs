{-# LANGUAGE UnicodeSyntax #-}

module T22 where

import Utils

import Prelude.Unicode
import Data.List ( isPrefixOf )
import Control.Arrow

import Data.Set ( Set, insert, member )
import Data.Foldable ( toList )

play ∷ [Int] → [Int] → (Bool, [Int])
play [] ys = (False, ys)
play xs [] = (True,  xs)
play (x:xs) (y:ys)
  | x > y     = play (xs ++ [x, y]) ys
  | y > x     = play xs (ys ++ [y, x])
  | otherwise = error $ "play " <> show (xs, ys)

recplay ∷ [Int] → [Int] → (Bool, [Int])
recplay = go mempty
  where
    go ∷ Set ([Int], [Int]) → [Int] → [Int] -> (Bool, [Int])
    go _ [] ys = (False, ys)
    go _ xs [] = (True,  xs)
    go seen xss@(x : xs) yss@(y : ys)
      | (xss, yss) `member` seen      = (True, xss)
      | length xs ≥ x ∧ length ys ≥ y = win . fst $ recplay (take x xs) (take y ys)
      | otherwise = win (x > y)
      where
        win True = next (xs ++ [x, y]) ys
        win False = next xs (ys ++ [y, x])
        next = go ((xss, yss) `insert` seen)

main ∷ IO ()
main = do
    (a, b) ← parse <$> getContents
    print . score $ play a b
    print . score $ recplay a b
  where
    score ∷ (Foldable f, Integral n) ⇒ (a, f n) → n
    score = sum . zipWith (*) [1..] . reverse . toList . snd
    parse ∷ String → ([Int], [Int])
    parse = lines
        >>> filter (≠ "")
        >>> splitsBy ("Player" `isPrefixOf`)
        >>> (\[[], a, b] → (map read a, map read b))
