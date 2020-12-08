{-# LANGUAGE UnicodeSyntax, TypeApplications, ScopedTypeVariables, ViewPatterns, BangPatterns #-}

module T07 where

import Prelude.Unicode
import Data.List
import Data.Maybe
import Data.Foldable ( toList )
import Data.Char
import Data.Tuple ( swap )
import Data.Monoid
import Control.Monad
import Control.Arrow

import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Map ( Map, mapKeys, foldrWithKey' )
import qualified Data.Map as Map
import Data.Vector ( Vector, imap )
import qualified Data.Vector as Vector

import Utils
import Indexable

import Debug.Trace

contained ∷ ∀κ ι. (Num ι, Eq ι, Show ι) ⇒ Vector (Map Int ι) → Int → Vector ι
contained !rules !what = fixpt indirectly directly
  where
    directly ∷ Vector ι
    directly = fmap (fromMaybe 0 . (!? what)) rules

    indirectly ∷ Vector ι → Vector ι
    indirectly !prev = imap compose rules
      where
        compose i = foldrWithKey' (\k n s → prev ! k * n + s) (directly ! i)


main ∷ IO ()
main = do
    rules' ← parse <$> getContents
    let colours ∷ Set String
        colours = Set.fromList (map fst rules')

        colourCode ∷ Map String Int
        colourCode = Map.fromList (zip (toList colours) [0..])

        rules = Vector.fromList . map snd . sort $ map ((colourCode !) *** mapKeys (colourCode !)) rules'

        ct = contained rules (colourCode ! "shiny gold")

    print $ count (≥ 1) ct
  where
    parse = fmap parseLine . lines
    parseLine = (unwords . init *** parseBags) . break (≡ "contain") . words
    parseBags = maybe Map.empty Map.fromList . mapM parseBag . splitsBy (≡ ',') . unwords . drop 1
    parseBag = readBag . rtrim (\x → x ≡ "bag" ∨ x ≡ "bags") . words . rtrim (≡ '.') . ltrim isSpace
    readBag ("no":_) = Nothing
    readBag (x:xs)   = Just (unwords xs, read @Int x)
