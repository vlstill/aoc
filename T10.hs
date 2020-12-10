{-# LANGUAGE UnicodeSyntax, TypeApplications, TemplateHaskell, ViewPatterns #-}

module T10 where

import Utils
import Indexable

import Prelude.Unicode
import Data.List

countConnections ∷ Int → [Int] → Int → Integer
countConnections from [] (subtract from → diff) = fromIntegral . fromEnum $ diff ≥ 0 && diff ≤ 3
countConnections from (x@(subtract from → diff):xs) to
    | diff < 0 ∨ diff > 3 = 0
    | otherwise = countConnections x xs to + countConnections from xs to

main ∷ IO ()
main = do
    adapters <- sort . fmap (read @Int) . lines <$> getContents
    let dev = maximum adapters + 3
        rates = 0 : adapters ++ [dev]
        diffs = zipWith (-) (tail rates) rates
    print $ count (≡ 1) diffs * count (≡ 3) diffs
    print $ countConnections 0 adapters dev
