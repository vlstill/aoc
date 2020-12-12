{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables, BangPatterns, TemplateHaskell, ViewPatterns #-}

module T12 where

import Utils
import Indexable

import Prelude.Unicode
import Data.Monoid
import Data.Vector ( Vector, fromList, imap )
import Control.Arrow
import Control.Lens ( makeLenses, (%~), (-~), (+~), view, (&) )
import Data.Default.Class

import Debug.Trace

data Direction = E | S | W | N | L | R | F deriving ( Eq, Show, Read, Enum )
type Action = (Direction, Int)

parseAction ∷ String → Action
parseAction (x:xs) = (read [x], read xs)
parseAction _      = error "empty action"

data ShipPosition = ShipPosition { _eastWest ∷ Int, _northSouth ∷ Int, _heading ∷ Direction } deriving Show
makeLenses ''ShipPosition

instance Default ShipPosition where
    def = ShipPosition { _northSouth = 0, _eastWest = 0, _heading = E }

manhattanFromCenter ∷ ShipPosition → Int
manhattanFromCenter ship = abs (view northSouth ship) + abs (view eastWest ship)

navigage ∷ ShipPosition → Action → ShipPosition
navigage ship = uncurry go
  where
    go ∷ Direction → Int → ShipPosition
    go F x = go (view heading ship) x
    go R x = ship & heading %~ rotate x
    go L x = ship & heading %~ rotate (-x)
    go W x = ship & eastWest -~ x
    go E x = ship & eastWest +~ x
    go S x = ship & northSouth -~ x
    go N x = ship & northSouth +~ x

    rotate ∷ Int → Direction → Direction
    rotate x@((`mod` 90) → 0) = toEnum . rot (x `div` 90) . fromEnum
    rotate x     = error $ "rotate: " <> show x

    rot x y = (y + x) `mod` 4

main ∷ IO ()
main = do
    navigInstr ← fmap parseAction . lines <$> getContents
    print . manhattanFromCenter $ foldl navigage def navigInstr
    pure ()
