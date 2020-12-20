{-# LANGUAGE LambdaCase, UnicodeSyntax, TypeApplications, ViewPatterns, TupleSections #-}

module T20 where

import Utils
import Indexable

import Prelude.Unicode
import Data.List
import Data.Function
import Data.Tuple
import Control.Arrow

import Debug.Trace

data Tile = Tile { img ∷ [[Bool]], fingerprints ∷ [RList Bool] } deriving (Show, Eq)
newtype RList α = RList [α] deriving (Show)

instance Eq α ⇒ Eq (RList α) where
    RList a == RList b = a == b || reverse a == b

tile ∷ [String] → (Int, Tile)
tile (x:xs) = (read (init (words x ! 1)), mk (map (≡ '#') <$> xs))
  where
    mk raw@(transpose → traw) = Tile { img = raw,
                                       fingerprints = RList <$> [head raw, last raw, head traw, last traw]
                                     }

main = do
    tiles ← map tile . splitsBy (≡ "") . lines <$> getContents
    print tiles
    let fingers = ($ tiles)
                    $ concatMap (second fingerprints >>> \(i, fs) → map (, i) fs)
                  >>> groupByUnsorted ((==) `on` fst)
                  >>> map (map snd)
                  >>> concatMap (\xs@(length → l) → map (, l - 1) xs)
                  >>> sort
                  >>> groupBy ((==) `on` fst)
                  >>> map (\xs@((i, _):_) → (i, sum (map snd xs)))
                  >>> map swap
                  >>> sort
    print . product . map snd . filter ((≡ 2) . fst) $ fingers
