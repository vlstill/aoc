{-# LANGUAGE UnicodeSyntax, TypeApplications, ViewPatterns, LambdaCase, GeneralisedNewtypeDeriving #-}

module T24 where

import Utils

import Prelude.Unicode
import Data.List ( isPrefixOf )
import Data.Int
import Data.Maybe
import Data.Char
import Data.Complex
import Data.AEq
import Data.Function
import Text.Read
import Control.Arrow
import Control.Applicative

import Data.Set ( Set, insert, member )
import Data.Foldable ( toList )

data Direction = E | SE | SW | W | NW | NE deriving (Eq, Show, Read, Enum)

parse ∷ String → [Direction]
parse = go . map toUpper
  where
    go [] = []
    go xs = fromMaybe (error $ "go " <> show xs) $ parseAndGo 1 xs <|> parseAndGo 2 xs

    parseAndGo n xs = (: go (drop n xs)) <$> readMaybe @Direction (take n xs)

newtype Vec = Vec { unVec :: Complex Double } deriving (Show, Num)

instance Eq Vec where
  (==) = (~==) `on` unVec

dirVec ∷ Direction → Vec
dirVec = Vec . \case
          E  → 1 :+ 0
          SE → 0.5 :+ (-v)
          SW → (-0.5) :+ (-v)
          W  → - unVec (dirVec E)
          NW → - unVec (dirVec SE)
          NE → - unVec (dirVec SW)
  where
    v = sqrt (1 - 0.25)

flipTile ∷ [Vec] → Vec → [Vec]
flipTile flipped tile
  | tile ∈ flipped = filter (≠ tile) flipped
  | otherwise      = tile : flipped

main ∷ IO ()
main = do
    instructions ← map parse . lines <$> getContents
    let tiles = map (sum . map dirVec) instructions
        flipped = foldl flipTile [] tiles
    print $ length flipped
