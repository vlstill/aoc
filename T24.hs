{-# LANGUAGE UnicodeSyntax, TypeApplications, LambdaCase #-}

module T24 where

import Utils

import Prelude.Unicode
import Data.Maybe
import Data.Char
import Data.Foldable ( toList )
import Text.Read ( readMaybe )
import Control.Applicative
import System.IO

import Data.Set ( Set, insert, member, notMember )
import qualified Data.Set as Set

data Direction = E | SE | SW | W | NW | NE deriving (Eq, Show, Read, Enum)

parse ∷ String → [Direction]
parse = go . map toUpper
  where
    go [] = []
    go xs = fromMaybe (error $ "go " <> show xs) $ parseAndGo 1 xs <|> parseAndGo 2 xs

    parseAndGo n xs = (: go (drop n xs)) <$> readMaybe @Direction (take n xs)

newtype HexVec = HexVec { unHexVec :: (Int, Int) } deriving (Show, Eq, Ord)

instance Semigroup HexVec where
    HexVec (a, b) <> HexVec (c, d) = HexVec (a + c, b + d)

instance Monoid HexVec where
    mempty = HexVec (0, 0)

dirHexVec ∷ Direction → HexVec
dirHexVec = HexVec . \case
              E  → (1, 0)
              SE → (0, 1)
              SW → (-1, 1)
              W  → (-1, 0)
              NW → (0, -1)
              NE → (1, -1)

flipTile ∷ Set HexVec → HexVec → Set HexVec
flipTile flipped tile
  | tile `member` flipped = Set.filter (≠ tile) flipped
  | otherwise             = tile `insert` flipped

step ∷ Set HexVec → Set HexVec
step orig = Set.filter (\x → let bn = blackNeighbors x in 0 < bn && bn ≤ 2) orig
         <> Set.filter ((≡ 2) . blackNeighbors) next
  where
    next = Set.fromList [ x | x' ← toList orig, x ← neigh x', x `notMember` orig ]

    neigh x = map ((<> x) . dirHexVec) [toEnum 0 ..]
    blackNeighbors x = count @Int (`member` orig) (neigh x)

main ∷ IO ()
main = do
    instructions ← map parse . lines <$> getContents
    let tiles = map (mconcat . map dirHexVec) instructions
        flipped = foldl flipTile mempty tiles
    hSetBuffering stdout LineBuffering
    print $ length flipped
    print . length . (step `fpow` 100) $ flipped
