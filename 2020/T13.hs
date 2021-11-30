{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables #-}

module T13 where

import Utils

import Prelude.Unicode
import Data.List ( minimumBy )
import Data.Function ( on )
import Control.Arrow
import Control.Exception ( assert )

data ModEq ι = ModEq { val ∷ ι, modulo ∷ ι } deriving ( Show, Eq )

solve ∷ ∀ι. Integral ι ⇒ [ModEq ι] → ι
solve eqs = (`mod` mprod) . sum . map (\e → let m = mprod `div` modulo e in val e * m * modInv m (modulo e)) $ eqs
  where
    moduli = map modulo eqs
    mprod = assert (all (≡ 1) [ gcd a b | a ← moduli, b ← moduli, a ≠ b ])
                   (product moduli)

modInv ∷ ∀ι. Integral ι ⇒ ι → ι → ι
modInv a n = assert (g == 1) $ (x + n) `mod` n
  where
    (x, _, g) = egcd a n

egcd :: ∀ι. Integral ι ⇒ ι -> ι -> (ι, ι, ι)
egcd a 0 = (1, 0, a)
egcd a b = let (q, r) = a `quotRem` b
               (s, t, g) = egcd b r
           in (t, s - q * t, g)

parse ∷ String → (Integer, [(Integer, Integer)])
parse = (read *** parseBuses) . break (≡ '\n')
  where
    parseBuses = map (second read) . filter ((≠ "x") . snd) . zip [0..] . splitsBy (≡ ',')

main ∷ IO ()
main = do
    (from, busesTS) ← parse <$> getContents
    let buses = map snd busesTS
    let dueIn x = x - (from `mod` x)
    let earliest = minimumBy (compare `on` dueIn) buses
    print (earliest * dueIn earliest)
    let eqs = map (\(i, b) → ModEq (b - i) b) busesTS
    print $ solve eqs
