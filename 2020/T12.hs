{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables, BangPatterns, TemplateHaskell, ViewPatterns #-}

module T12 where

import Control.Lens
import Data.Default.Class
import Control.Monad.State

data Direction = E | S | W | N | L | R | F deriving ( Eq, Show, Read, Enum )
type Action = (Direction, Int)

parseAction ∷ String → Action
parseAction (x:xs) = (read [x], read xs)
parseAction _      = error "empty action"

data ShipPosition = ShipPosition { _eastWest ∷ Int, _northSouth ∷ Int, _heading ∷ Direction } deriving Show
makeLenses ''ShipPosition

data NaviPosition = NaviPosition { _wpEW ∷ Int, _wpNS ∷ Int, _sEW ∷ Int, _sNS ∷ Int } deriving Show
makeLenses ''NaviPosition

instance Default ShipPosition where
    def = ShipPosition { _northSouth = 0, _eastWest = 0, _heading = E }

instance Default NaviPosition where
    def = NaviPosition { _wpEW = 10, _wpNS = 1, _sEW = 0, _sNS = 0 }

manhattanBy ∷ ∀σ. Getting Int σ Int → Getting Int σ Int → σ → Int
manhattanBy ns ew ship = abs (view ns ship) + abs (view ew ship)

navigage ∷ Action → State ShipPosition ()
navigage = uncurry go
  where
    go ∷ Direction → Int → State ShipPosition ()
    go F x = use heading >>= flip go x
    go R x = heading %= rotate x
    go L x = heading %= rotate (-x)
    go W x = eastWest -= x
    go E x = eastWest += x
    go S x = northSouth -= x
    go N x = northSouth += x

    rotate ∷ Int → Direction → Direction
    rotate ((`divMod` 90) → (x, 0)) = toEnum . (`mod` 4) . (+ x) . fromEnum
    rotate x     = error $ "rotate: " <> show x

navigage' ∷ Action → State NaviPosition ()
navigage' = uncurry go
  where
    go ∷ Direction → Int → State NaviPosition ()
    go N x = wpNS += x
    go S x = wpNS -= x
    go E x = wpEW += x
    go W x = wpEW -= x
    go L x = rotate (-x)
    go R x = rotate x
    go F x = do
              ns ← use wpNS
              ew ← use wpEW
              sNS += ns * x
              sEW += ew * x

    rotate ∷ Int → State NaviPosition ()
    rotate 0 = pure ()
    rotate n@(signum → s) = do
                ns ← use wpNS
                ew ← use wpEW
                wpNS .= (-s) * ew
                wpEW .= s * ns
                rotate (n - (90 * s))

main ∷ IO ()
main = do
    navigInstr ← fmap parseAction . lines <$> getContents
    print . manhattanBy northSouth eastWest $ execState (mapM_ navigage navigInstr) def
    print . manhattanBy sNS sEW $ execState (mapM_ navigage' navigInstr) def
