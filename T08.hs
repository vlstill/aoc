{-# LANGUAGE UnicodeSyntax, TypeApplications, TemplateHaskell, ViewPatterns #-}

module T08 where

import Utils
import Indexable

import Prelude.Unicode
import Data.List
import Data.Monoid
import Data.Char
import Data.Vector ( Vector, fromList )
import Data.Function ( (&) )
import Debug.Trace

import Control.Lens ( makeLenses, Lens', (.~), (^.), (%~), view )

data Instr = Nop Integer
           | Acc Integer
           | Jmp Int
           deriving ( Show, Read )

data Machine = Machine { _instrs ∷ Vector Instr, _ip ∷ Int, _accum ∷ Integer }
               deriving ( Show )
makeLenses ''Machine

initMachine ∷ Vector Instr → Machine
initMachine instrs = Machine { _instrs = instrs, _ip = 0, _accum = 0 }

step ∷ Machine → Machine
step mach = mach & case view instrs mach !? view ip mach of
    Just (Nop _) -> next
    Just (Acc v) -> next . (accum %~ (+ v))
    Just (Jmp v) -> ip %~ (+ v)
    Nothing -> error "Instruction out of range"
  where
    next = ip %~ (+ 1)

runUntilCycle ∷ Machine → Machine
runUntilCycle = go []
  where
    go seen mach@(step → next@(view ip → nip))
      | nip ∈ seen = mach
      | otherwise  = go (nip:seen) next

parseCode ∷ String → Vector Instr
parseCode = fromList . fmap parseInst . lines
  where
    parseInst = read @Instr . unwords . htmap (htmap toUpper id) fixSign . words
    fixSign xs@('-':_) = embrace xs
    fixSign ('+':xs)   = xs

main ∷ IO ()
main = do
     code <- parseCode <$> getContents
     print . view accum . runUntilCycle . initMachine $ code
