{-# LANGUAGE UnicodeSyntax, TypeApplications #-}

module T05 where

import Prelude.Unicode

data Direction = F | B | L | R deriving ( Show, Eq, Read, Enum )

parseTickets :: String → [[Direction]]
parseTickets = fmap (fmap (read @Direction . (:[]))) . lines

ticketIDs ∷ [[Direction]] → [Integer]
ticketIDs = map (foldl accDigits 0)
  where
    accDigits acc dig = acc * 2 + fromIntegral (fromEnum dig) `mod` 2

main ∷ IO ()
main = do
    tickets ← parseTickets <$> getContents
    let ids = ticketIDs tickets
    print . maximum $ ids
    print . only . filter (\x → x `notElem` ids ∧ (x + 1) `elem` ids ∧ (x - 1) `elem` ids) $ [0 .. 128 * 8]
  where
    only [x] = x
    only xs  = error $ "Found more than one element: " <> show xs
