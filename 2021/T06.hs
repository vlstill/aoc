{-# LANGUAGE TypeApplications, BangPatterns #-}

module T06 where

import Control.Arrow
import Data.List
import Data.Monoid
import Data.Function ( on )

main = do
    input <- map toCnt . group . sort .  map (read @Integer) . words . map c2s <$> getContents
    let res x = sum . map snd . (!! x) $ process input
    -- print (last . take 18 $ process input)
    -- print (res 18)
    print (res 80)
    print (res 256)
  where
    c2s ',' = ' '
    c2s x   = x
    toCnt xs@(x:_) = (x, genericLength xs)

process :: [(Integer, Integer)] -> [[(Integer, Integer)]]
process = iterate produce

produce :: [(Integer, Integer)] -> [(Integer, Integer)]
produce !x = collect (concatMap (spawn . first (subtract 1)) x)
  where
    spawn (-1, !cnt) = [(6, cnt), (8, cnt)]
    spawn x          = [x]

    collect :: [(Integer, Integer)] -> [(Integer, Integer)]
    collect = sort >>> groupBy ((==) `on` fst) >>> map (\xs@((x, _):_) -> (x, sum (map snd xs)))
