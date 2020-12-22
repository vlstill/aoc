{-# LANGUAGE LambdaCase, UnicodeSyntax, TypeApplications, ScopedTypeVariables, ViewPatterns, TupleSections #-}

module T22 where

import Utils
import Indexable

import Prelude hiding ( take )
import Prelude.Unicode
import Data.List ( isPrefixOf )
import Control.Arrow
import Control.Monad.State.Strict

import Data.Sequence ( Seq ( Empty, (:<|), (:|>) ), fromList, take )
import Data.Set ( Set, insert, member )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Foldable ( toList )

import Debug.Trace

play ∷ [Int] → [Int] → (Bool, [Int])
play [] ys = (False, ys)
play xs [] = (True,  xs)
play (x:xs) (y:ys)
  | x > y     = play (xs ++ [x, y]) ys
  | y > x     = play xs (ys ++ [y, x])
  | otherwise = error $ "play " <> show (xs, ys)

type Cache = Map (Seq Int, Seq Int) (Bool, Seq Int)

recplay ∷ Seq Int → Seq Int → (Bool, Seq Int)
recplay xs0 ys0 = evalState (rplay xs0 ys0) mempty
  where
    rplay ∷ Seq Int → Seq Int → State Cache (Bool, Seq Int)
    rplay xs ys = gets (‽ (xs, ys)) >>= \case
                    Just res → pure res
                    _ → do
                        res ← go mempty xs ys
                        modify (Map.insert (xs, ys) res)
                        pure res

    go ∷ Set (Seq Int, Seq Int) → Seq Int → Seq Int -> State Cache (Bool, Seq Int)
    go _ Empty ys = pure (False, ys)
    go _ xs Empty = pure (True,  xs)
    go seen xss@(x :<| xs) yss@(y :<| ys)
      | (xss, yss) `member` seen      = pure (True, xss)
      | length xs ≥ x ∧ length ys ≥ y = win . fst =<< rplay (take x xs) (take y ys)
      | otherwise = win (x > y)
      where
        win True = next (xs :|> x :|> y) ys
        win False = next xs (ys :|> y :|> x)
        next = go ((xss, yss) `insert` seen)

main ∷ IO ()
main = do
    (a, b) ← parse <$> getContents
    print . score $ play a b
    print . score $ recplay (fromList a) (fromList b)
  where
    score ∷ (Foldable f, Integral n) ⇒ (a, f n) → n
    score = sum . zipWith (*) [1..] . reverse . toList . snd
    parse ∷ String → ([Int], [Int])
    parse = lines
        >>> filter (≠ "")
        >>> splitsBy ("Player" `isPrefixOf`)
        >>> (\[[], a, b] → (map read a, map read b))
