{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow

main = do
    input <- map (read @Integer) . words . map c2s <$> getContents
    print (take 80 . map length $ process input)
  where
    c2s ',' = ' '
    c2s x   = x

process :: [Integer] -> [[Integer]]
process = tail . iterate produce

produce :: [Integer] -> [Integer]
produce = map (subtract 1) >>> concatMap spawn
  where
    spawn (-1) = [6, 8]
    spawn x    = [x]
