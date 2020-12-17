{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables, TypeApplications #-}

module T17 where

import Indexable
import Utils

import Prelude.Unicode
import Data.Tuple ( swap )
import Data.List ( transpose, isPrefixOf )
import Data.Foldable ( toList )
import Control.Arrow

import Data.Set ( Set, fromList, member )
import qualified Data.Set as Set

type PocketDimension = Set (Integer, Integer, Integer)

load ∷ String → PocketDimension
load = lines >>>
       map (zip [0..] >>> filter (snd >>> (≡ '#'))) >>>
       zip [0..] >>>
       concatMap (\(x, l) -> map (\(y, _) -> (x, y, 0)) l) >>>
       fromList

step ∷ PocketDimension → PocketDimension
step state = Set.filter active considered
  where
    sigs = [-1, 0, 1]
    considered = fromList [ (x + dx, y + dy, z + dz)
                          | (x, y, z) ← toList state, dx ← sigs, dy ← sigs, dz ← sigs ]
    active coo@(x, y, z)
        | coo `member` state = activeNeighbours ∈ [2, 3]
        | otherwise          = activeNeighbours ≡ 3
      where
        activeNeighbours = count @Int (`member` state) neightbours
        neightbours = [ (x + dx, y + dy, z + dz) | dx ← sigs, dy ← sigs, dz ← sigs,
                                                   (dx, dy, dz) ≠ (0, 0, 0) ]

main ∷ IO ()
main = do
    initial ← load <$> getContents
    print . length $ fpow step 6 initial

