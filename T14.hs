{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables, ViewPatterns, TemplateHaskell #-}

module T14 where

import Utils
import Indexable

import Prelude.Unicode

import Data.List ( minimumBy, foldl', uncons )
import Data.Maybe
import Data.Function ( on )
import Data.Map ( Map, empty, insert )
import Data.Bits
import Data.Int ( Int64 )
import Data.Default.Class

import Control.Arrow
import Control.Exception ( assert )
import Control.Monad.State ( execState, State )
import Control.Lens

import Text.Regex.Base ()
import Text.Regex.PCRE ( (=~) )

data Mask = M { andMask ∷ Int64, orMask ∷ Int64 } deriving (Eq, Show)

parseMask ∷ String → Mask
parseMask m = M { andMask = toMask "0" "X1" m, orMask = toMask "0X" "1" m }
  where
    toMask zero one = foldl' addBit 0
      where
        addBit bits bit
          | bit ∈ zero = bits * 2
          | bit ∈ one  = bits * 2 + 1
          | otherwise  = error $ "Unknown bit " <> [bit]

applyMask ∷ Int64 → Mask → Int64
applyMask x m = (x .&. andMask m) .|. orMask m

reMask, reMem ∷ String
reMask = "mask = ([X01]*)"
reMem  = "mem\\[([0-9]*)\\] = ([0-9]*)"

data Instruction = Mem Int64 Int64
                 | Mask Mask
                 deriving ( Show, Eq )

captGs ∷ [Int] → [[String]] → Maybe [String]
captGs ixs [xs] = mapM (xs !?) ixs
captGs _    _    = Nothing

parseInstr ∷ String → Instruction
parseInstr ((=~ reMask) >>> captGs [1] → Just [m]) = Mask (parseMask m)
parseInstr ((=~ reMem) >>> captGs [1, 2] → Just [addr, val]) =  Mem (read addr) (read val)
parseInstr inst = error $ "parseInstr: " <> inst

data MachineState = MachineState { _mask ∷ Mask, _memory ∷ Map Int64 Int64 } deriving Show
makeLenses ''MachineState

instance Default MachineState where
    def = MachineState (M (complement 0) 0) empty

run ∷ [Instruction] → MachineState
run inst = execState (mapM_ go inst) def
  where
    go ∷ Instruction → State MachineState ()
    go (Mask m) = mask .= m
    go (Mem addr val) = use mask >>= \m → memory %= insert addr (applyMask val m)

main ∷ IO ()
main = do
    prog <- fmap parseInstr . lines <$> getContents
    print . sum . view memory $ run prog
