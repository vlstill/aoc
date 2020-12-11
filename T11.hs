{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables #-}

module T11 where

import Utils
import Indexable

import Prelude.Unicode
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
    parseSeat x   = error $ "unknow seat " <> show x

type Coords = (Int, Int)
type Dir = (Int, Int)

neighbours ∷ ∀μ. Monoid μ ⇒ (SeatMap → Coords → Dir → Maybe Seat) → (Seat → μ) → (Int, Int) → SeatMap → μ
neighbours select proj coo sm = foldMap (maybe mempty proj . select sm coo) dirs
  where
    dirs = [ (sx, sy) | sx <- sigs, sy <- sigs, (sx, sy) ≠ (0, 0) ]
    sigs = [-1, 0, 1]

directNeight ∷  SeatMap -> Coords -> Dir -> Maybe Seat
directNeight sm (x, y) (dx, dy) = sm ‽ (x + dx) >>= (‽ (y + dy))

visibleNeigh ∷ SeatMap -> Coords -> Dir -> Maybe Seat
visibleNeigh sm (x0, y0) (dx, dy) = go (x0 + dx, y0 + dy)
  where
    go (x, y) = case sm ‽ x >>= (‽ y) of
        Nothing    → Nothing
        Just Floor → go (x + dx, y + dy)
        s          → s

mapMap ∷ (Seat → (Int, Int) → Seat) → SeatMap → SeatMap
mapMap f = imap rowMap
  where
    rowMap :: Int → Vector Seat → Vector Seat
    rowMap x = imap (\y s → f s (x, y))

step ∷ (SeatMap → Coords → Dir → Maybe Seat) → Int → SeatMap → SeatMap
step neighbourSelect occupThresh sm = mapMap seatStep sm
  where
    seatStep Floor _ = Floor
    seatStep Empty coo
      | occupNei coo ≡ 0 = Occupied
      | otherwise        = Empty
    seatStep Occupied coo
      | occupNei coo ≥ occupThresh = Empty
      | otherwise                  = Occupied

    occupNei coo = getSum $ neighbours neighbourSelect (Sum . fromEnum . (≡ Occupied)) coo sm


occupied ∷ SeatMap → Int
occupied = getSum . foldMap (foldMap (Sum . fromEnum . (≡ Occupied)))

main ∷ IO ()
main = do
    seatMap <- parseMap <$> getContents
    print . occupied $ fixpt (step directNeight 4) seatMap
    print . occupied $ fixpt (step visibleNeigh 5) seatMap
