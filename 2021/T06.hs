{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow
import Data.List
import Data.Monoid

main = do
    input <- map toCnt . group . sort .  map (read @Integer) . words . map c2s <$> getContents
    let res = map (foldMap (Sum . snd)) $ process input
    print (take 80 res)
    print (take 256 res)
  where
    c2s ',' = ' '
    c2s x   = x
    toCnt xs@(x:_) = (x, genericLength xs)

process :: [(Integer, Integer)] -> [[(Integer, Integer)]]
process = tail . iterate produce

produce :: [(Integer, Integer)] -> [(Integer, Integer)]
produce = map (first (subtract 1)) >>> concatMap spawn
  where
    spawn (-1, cnt) = [(6, cnt), (8, cnt)]
    spawn x    = [x]
