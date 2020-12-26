{-# LANGUAGE UnicodeSyntax, TypeApplications #-}

module T25 where

import Utils

import Prelude.Unicode
import Data.List ( isPrefixOf )
import Data.Int
import Data.Maybe
import Control.Arrow
import Control.Applicative

import Data.Set ( Set, insert, member )
import Data.Foldable ( toList )

trans ∷ Integer → Integer → Integer
trans sn x = (sn * x) `mod` 20201227

crackLoop ∷ Integer → Integer → Integer
crackLoop sn tgt = fst . head . filter ((≡ tgt) . snd) $ zip [0..] (iterate (trans sn) 1)

main ∷ IO ()
main = do
    [cardp, doorp] ← map (read @Integer) . lines <$> getContents
    let cardSec = crackLoop 7 cardp
        doorSec = crackLoop 7 doorp
    print $ fpow (trans doorp) cardSec 1
