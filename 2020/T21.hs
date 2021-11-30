{-# LANGUAGE LambdaCase, UnicodeSyntax, TypeApplications, ScopedTypeVariables, ViewPatterns, TupleSections #-}

module T21 where

import Utils
import Indexable

import Prelude.Unicode
import Data.List ( sort, groupBy, transpose, sortBy, find, intersect, nub, (\\) )
import Data.Function
import Data.Tuple
import Control.Arrow
import Control.Applicative

import Data.Map ( Map, fromList, fromListWith )

import Debug.Trace

shortestFirst ∷ ∀α β. [(α, [β])] → [(α, [β])]
shortestFirst = sortBy (compare `on` (length . snd))

coalesce ∷ ∀α β. (Eq α, Eq β) ⇒ [(α, [β])] → [(α, [β])]
coalesce (x@(al, is) : xs)
  | Just is2 ← lookup al xs = (al, is `intersect` is2) : coalesce (filter ((≠ al) . fst) xs)
  | otherwise             = x : coalesce xs
coalesce [] = []

prune ∷ ∀α β. (Eq α, Eq β, Show α, Show β) ⇒ [(α, [β])] → Maybe [(α, β)]
prune [] = Just []
prune ((al, is) : als) = foldr (<|>) Nothing $ map go is
  where
    go ∷ β → Maybe [(α, β)]
    go ing = ((al, ing) :) <$> (prune . shortestFirst . map (second (filter (≠ ing))) $ als)

main ∷ IO ()
main = do
    rawAllegens ← parse <$> getContents
    let allergens = nub . concatMap fst $ rawAllegens
    print allergens
    let candidates = shortestFirst . coalesce $ concatMap (\(al, ing) → map (, ing) al) rawAllegens
        Just allergenMap = prune candidates
        known = map snd allergenMap
        unknown = filter (`notElem` known) $ concatMap snd rawAllegens
    print allergenMap
    print $ length unknown
  where
    parse = filter (≠ ',')
        >>> lines
        >>> map (splitsBy (\x → x ≡ '(' ∨ x ≡ ')')
                 >>> (\[x, y] → (x, y))
                 >>> (words *** (words >>> filter (≠ "contains")))
                 >>> swap)
