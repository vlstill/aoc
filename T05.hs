{-# LANGUAGE UnicodeSyntax, TypeApplications, NamedFieldPuns #-}

module T05 where

import Prelude.Unicode

data Coord = Coord { row ∷ Integer, col ∷ Integer } deriving ( Show, Eq )
data Dimen = Dimen { rows ∷ Integer, cols ∷ Integer } deriving ( Show, Eq )
data Direction = F | B | L | R deriving ( Show, Eq, Read )

parseTickets :: String → [[Direction]]
parseTickets = fmap (fmap (read @Direction . (:[]))) . lines

coors :: Dimen → [Direction] → Coord
coors Dimen { rows, cols } = uncurry co . foldl go ((0, rows - 1), (0, cols - 1))
  where
    co ∷ (Integer, Integer) → (Integer, Integer) → Coord
    co (r, r1) (c, c1) = Coord { row = r, col = c }

    go ∷ ((Integer, Integer), (Integer, Integer)) → Direction → ((Integer, Integer), (Integer, Integer))
    go (rr, cr) F = (step rr False, cr)
    go (rr, cr) B = (step rr True, cr)
    go (rr, cr) L = (rr, step cr False)
    go (rr, cr) R = (rr, step cr True)

    step (a, b) False = (a, a + (b - a) `div` 2)
    step (a, b) True  = (a + (b - a) `div` 2 + 1, b)

seatID ∷ Coord → Integer
seatID Coord { row, col } = row * 8 + col

ticketID ∷ Dimen → [Direction] → Integer
ticketID dim dir = seatID $ coors dim dir

ticketIDs ∷ Dimen → [[Direction]] → [Integer]
ticketIDs dim = map (ticketID dim)


main ∷ IO ()
main = do
    tickets ← parseTickets <$> getContents
    let ids = ticketIDs Dimen { rows = 128, cols = 8 } tickets
    print . maximum $ ids
    print . only . filter (\x → x `notElem` ids ∧ (x + 1) `elem` ids ∧ (x - 1) `elem` ids) $ [0 .. 128 * 8]
  where
    only [x] = x
