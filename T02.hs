{-# LANGUAGE UnicodeSyntax, ViewPatterns #-}

module T01a where

import Prelude ( IO, Int, Char, String, Bool, Show, Eq, Ord,
                 pure, (.), (<$>), ($), dropWhile, span, drop, lines, words,
                 read, uncurry, fromEnum, filter, subtract, (!!),
                 getContents, print )
import Prelude.Unicode
import Data.Char ( isSpace )
import Data.Foldable ( foldMap, length )
import Data.Monoid
import Data.Bool.Unicode
import Control.Arrow

data Policy = Policy (Int, Int) Char deriving (Show, Eq)

parseRange ∷ String → (Int, Int)
parseRange = (read *** read . drop 1) . span (≠ '-')

validate ∷ Policy → String → Bool
validate (Policy (f, t) c) pass = f ≤ cnt ∧ cnt ≤ t
  where
    cnt = length $ filter (≡ c) pass

countWith v = getSum . foldMap (Sum . fromEnum . uncurry v)

validate' ∷ Policy → String → Bool
validate' (Policy (subtract 1 → a, subtract 1 → b) c) pass = pass !! a ≡ c ⊻ pass !! b ≡ c

main ∷ IO ()
main = do
    input ← (parse <$>) . lines <$> getContents
    print $ countWith validate input
    print $ countWith validate' input

  where
    parse :: String → (Policy, String)
    parse = ((\[a,[b]] → Policy (parseRange a) b) . words *** dropWhile isSpace . drop 1) . span (≠ ':') 
