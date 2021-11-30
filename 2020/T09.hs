{-# LANGUAGE UnicodeSyntax, TypeApplications, TemplateHaskell, ViewPatterns, ScopedTypeVariables, PolyKinds, DataKinds, AllowAmbiguousTypes #-}

module T09 where

import GHC.TypeLits
import Data.Default.Class
import Data.Proxy
import Data.Maybe
import Control.Lens
import Control.Monad.State
import Control.Arrow

import Prelude.Unicode

newtype XMAS (β ∷ Nat) = XMAS { _buffer ∷ [Int] } deriving ( Show, Eq )
makeLenses ''XMAS

instance Default (XMAS β) where
    def = XMAS []

testIn ∷ [Int]
testIn = [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]

firstError ∷ ∀β. KnownNat β ⇒ [Int] → Maybe Int
firstError xs0 = evalState (go xs0) (def @(XMAS β))
  where
    go ∷ [Int] → State (XMAS β) (Maybe Int)
    go [] = pure Nothing
    go (x:xs) = use buffer >>= \buf → case length buf of
        l | l ≡ size → case [ () | a ← buf, b ← buf, a ≠ b, a + b ≡ x ] of
                        [] → pure $ Just x
                        _  → do
                            buffer %= take size . (x:)
                            go xs
        _ → buffer %= (x:) >> go xs

    size = fromInteger $ natVal @β Proxy

continuousSubrangeWithSum ∷ ∀α. (Ord α, Num α, Show α) ⇒ [α] → α → Maybe [α]
continuousSubrangeWithSum xss0 s = go [] xss0
  where
    go candidates@(sum → currentSum) xss
      | currentSum ≡ s, length candidates ≥ 2 = Just $ reverse candidates
      | currentSum < s, (x:xs) ← xss          = go (x : candidates) xs
      | currentSum > s                        = go (init candidates) xss
      | otherwise                             = Nothing

main ∷ IO ()
main = do
    numbers ← map (read @Int) . lines <$> getContents
    let Just erNum = firstError @25 numbers
    print erNum
    print . fromJust $ uncurry (+) . (minimum &&& maximum) <$> continuousSubrangeWithSum numbers erNum
