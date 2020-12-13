{-# LANGUAGE UnicodeSyntax, TypeApplications #-}

module T13 where

import Utils
import Indexable

import Prelude.Unicode
import Data.List ( sort, foldl', minimumBy )
import Data.Map.Strict ( Map, singleton, insert, toAscList )
import Data.Function ( on )
import Control.Monad.State.Strict
import Control.Arrow


main ∷ IO ()
main = do
    (from, buses) ← (read @Int *** map (read @Int) . filter (≠ "x") . splitsBy (≡ ',')) . break (≡ '\n') <$> getContents
    let dueIn x = x - (from `mod` x)
    let earliest = minimumBy (compare `on` dueIn) buses
    print (earliest * dueIn earliest)
