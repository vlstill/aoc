{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables, TypeApplications, DataKinds, KindSignatures #-}

module T17 where

import Utils

import Prelude.Unicode
import Data.Foldable ( toList )
import Data.Proxy
import Control.Arrow

import GHC.TypeLits

import Data.Set ( Set, fromList, member )
import qualified Data.Set as Set

newtype PocketDimension (δ ∷ Nat) = PD { unPD :: Set [Integer] }

load ∷ ∀δ. KnownNat δ ⇒ String → PocketDimension δ
load = lines >>>
       map (zip [0..] >>> filter (snd >>> (≡ '#'))) >>>
       zip [0..] >>>
       concatMap (\(x, l) -> map (\(y, _) -> lift x y) l) >>>
       fromList >>>
       PD
  where
    lift ∷ Integer → Integer → [Integer]
    lift x y = take d ([x, y] <> repeat 0)

    d = fromInteger $ natVal (Proxy @δ)

step ∷ ∀δ. KnownNat δ ⇒ PocketDimension δ → PocketDimension δ
step (PD state) = PD $ Set.filter active considered
  where
    d = fromInteger $ natVal (Proxy @δ)
    sigs = [-1, 0, 1]

    diffs ∷ Int → [[Integer]]
    diffs 1 = map (:[]) sigs
    diffs n = (:) <$> sigs <*> diffs (n - 1)

    considered = fromList [ zipWith (+) coo diff | coo ← toList state, diff ← diffs d ]

    active coo
        | coo `member` state = activeNeighbours ∈ [2, 3]
        | otherwise          = activeNeighbours ≡ 3
      where
        activeNeighbours = count @Int (`member` state) neightbours
        neightbours = [ zipWith (+) coo diff | diff ← diffs d, diff ≠ replicate d 0 ]

main ∷ IO ()
main = do
    initial ← getContents
    let calc ∷ ∀δ. KnownNat δ ⇒ PocketDimension δ → IO ()
        calc = print . length . unPD . fpow step 6
    calc $ load @3 initial
    calc $ load @4 initial

