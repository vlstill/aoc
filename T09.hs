{-# LANGUAGE UnicodeSyntax, TypeApplications, TemplateHaskell, ViewPatterns, ScopedTypeVariables, KindSignatures, PolyKinds, DataKinds #-}

module T09 where

import Utils
import Indexable

import Data.Kind ( Type )
import GHC.TypeLits
import Control.Lens
import Data.Default.Class
import Data.Proxy
import Control.Monad.State

import Prelude.Unicode

newtype XMAS (β ∷ Nat) = XMAS { _buffer ∷ [Int] } deriving ( Show, Eq )
makeLenses ''XMAS

instance Default (XMAS β) where
    def = XMAS []

testIn ∷ [Int]
testIn = [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]

firstError ∷ ∀β. KnownNat β ⇒ XMAS β → [Int] → Maybe Int
firstError buf xs = evalState (go xs) buf
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

main ∷ IO ()
main = do
    numbers ← map (read @Int) . lines <$> getContents
    print $ firstError (def @(XMAS 25)) numbers
