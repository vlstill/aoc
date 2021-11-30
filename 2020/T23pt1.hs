{-# LANGUAGE UnicodeSyntax, TypeApplications, ViewPatterns #-}

module T23 where

import Utils

import Prelude.Unicode
import Data.List ( isPrefixOf )
import Data.Int
import Data.Maybe
import Control.Arrow
import Control.Applicative

import Data.Set ( Set, insert, member )
import Data.Foldable ( toList )

type Ring = [Int32]

step ∷ Int32 → Ring → Ring
step maxCup@(subtract 1 → modCup) (current : a : b : c : rest) =
        (++ [current]) . fromMaybe (error "Cannot place") $ foldr (<|>) Nothing
        [ tryPlace [a, b, c] tgt rest
        | t ← [1..maxCup], let tgt = ((current - t - 1) `mod` modCup) + 1 ]
  where
    tryPlace picks@[a, b, c] at (x:xs)
      | at ≡ x    = Just (x : a : b : c : xs)
      | otherwise = (x :) <$> tryPlace picks at xs
    tryPlace _ _ []     = Nothing

main ∷ IO ()
main = do
    cups ← map (read . (: [])) . filter (≠ '\n') <$> getContents
    let part1 = fpow (step 9) 100 cups
        slice11 = ((++) <*> id)
              >>> dropWhile (≠ 1)
              >>> drop 1
              >>> takeWhile (≠ 1)
              >>> concatMap show
              >>> read @Integer
    print $ slice11 part1
    let part2max = 10 ^ 6
        part2cups = cups ++ [maximum cups + 1 .. part2max]
        part2 = fpow (step part2max) 10000000 part2cups
    print . product . take 2 . drop 1 . dropWhile (≠ 1) $ part2
