{-# LANGUAGE UnicodeSyntax, TypeApplications, TemplateHaskell, ViewPatterns, ScopedTypeVariables #-}

module T11 where

import Utils
import Indexable

import Prelude.Unicode
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Vector ( Vector, fromList, imap )

data Seat = Floor | Empty | Occupied deriving ( Eq, Ord, Show )

type SeatMap = Vector (Vector Seat)

parseMap ∷ String → SeatMap
parseMap = fromList . fmap (fromList . fmap parseSeat) . lines
  where
    parseSeat 'L' = Empty
    parseSeat '#' = Occupied
    parseSeat '.' = Floor

neighbours ∷ ∀μ. Monoid μ ⇒ (Seat → μ) → (Int, Int) → SeatMap → μ
neighbours proj (x, y) sm = foldMap (maybe mempty proj . at) nIdxs
  where
    nIdxs = [ (x + sx, y + sy) | sx <- sigs, sy <- sigs, (sx, sy) ≠ (0, 0) ]
    sigs = [-1, 0, 1]

    at (i, j) = sm ‽ i >>= (‽ j)

mapMap ∷ (Seat → (Int, Int) → Seat) → SeatMap → SeatMap
mapMap f = imap rowMap
  where
    rowMap :: Int → Vector Seat → Vector Seat
    rowMap x = imap (\y s → f s (x, y))

step ∷ SeatMap → SeatMap
step sm = mapMap seatStep sm
  where
    seatStep Floor _ = Floor
    seatStep Empty coo
      | occupNei coo ≡ 0 = Occupied
      | otherwise        = Empty
    seatStep Occupied coo
      | occupNei coo ≥ 4 = Empty
      | otherwise        = Occupied

    occupNei coo = getSum $ neighbours (Sum . fromEnum . (≡ Occupied)) coo sm

occupied ∷ SeatMap → Int
occupied = getSum . foldMap (foldMap (Sum . fromEnum . (≡ Occupied)))

main ∷ IO ()
main = do
    seatMap <- parseMap <$> getContents
    print . occupied $ fixpt step seatMap
