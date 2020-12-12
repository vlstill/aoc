{-# LANGUAGE UnicodeSyntax, TypeApplications #-}

module T10 where

import Utils
import Indexable

import Prelude.Unicode
import Data.List ( sort )
import Data.Map.Strict ( Map, singleton, insert, toAscList )
import Control.Monad.State.Strict

countConnections ∷ Int → [Int] → Int → Integer
countConnections from using to = execState (go (reverse (from : using))) (singleton to 1) ! from
  where
    go ∷ [Int] → State (Map Int Integer) ()
    go [] = pure ()
    go (x:xs) = do
        options ← gets $ sum . map snd . takeWhile ((≤ (x + 3)) . fst) . toAscList
        modify (insert x options)
        go xs

main ∷ IO ()
main = do
    adapters <- sort . fmap (read @Int) . lines <$> getContents
    let dev = maximum adapters + 3
        rates = 0 : adapters ++ [dev]
        diffs = zipWith (-) (tail rates) rates
    print @Integer $ count (≡ 1) diffs * count (≡ 3) diffs
    print $ countConnections 0 adapters dev
