{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables, ViewPatterns #-}

module T16 where

import Indexable
import Utils

import Prelude.Unicode

import Data.Tuple
import Data.List ( transpose )

import Control.Arrow

type Ticket = [Int]
type Rule = ([Range], String)

data Range = Range Int Int deriving (Show, Eq)

contains ∷ [Range] → Int → Bool
contains rs x = any (\(Range f t) → f ≤ x && x ≤ t) rs

parse ∷ String → ([Rule], Ticket, [Ticket])
parse = parsePatrs . splitsBy (≡ "") . lines
  where
    parsePatrs [r, y, n] = (map parseRule r, parseTicket (y ! 1), map parseTicket (drop 1 n))
    parsePatrs x = error $ "cannot parse: " ++ show x

    parseRule = break (≡ ':') >>> swap >>>
                first (drop 1 >>>
                       splitsBy (≡ ' ') >>>
                       filter ((&&) <$> (≠ "or" ) <*> (not . null)) >>>
                       map parseRange)
    parseRange = break (≡ '-') >>> second (drop 1) >>> (read *** read) >>> uncurry Range
    parseTicket = splitsBy (≡ ',') >>> map read

validAny ∷ [Rule] → Int → Bool
validAny rules val = any (`validFor` val) rules

validFor ∷ Rule → Int → Bool
validFor (rs, _) val = rs `contains` val

filterRules ∷ [Rule] → [Int] → [Rule]
filterRules rs vs = filter (\r → all (r `validFor`) vs) rs

elimitate ∷ [[String]] → [[String]]
elimitate = fixpt elim
  where
    elim ∷ [[String]] → [[String]]
    elim fields@(filter ((≡ 1) . length) >>> map head → singletons) = map (prune singletons) fields
    prune _         [f] = [f]
    prune singletons fs = filter (∉ singletons) fs

single ∷ ∀α. [α] → Maybe α
single [x] = Just x
single _   = Nothing

main ∷ IO ()
main = do
    (rules, your, nearby) ← parse <$> getContents
    print . sum $ filter (not . validAny rules) (concat nearby)
    let validNearby = filter (all (validAny rules)) nearby
        fieldValues = transpose validNearby
        fields' = map (map snd) $ map (filterRules rules) fieldValues
        Just fields = mapM single $ elimitate fields'
        your' = zip fields your
        departures = map snd . filter ((≡ "departure") . head . words . fst) $ your'
    print $ product departures
    
