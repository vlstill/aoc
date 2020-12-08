{-# LANGUAGE UnicodeSyntax, TypeApplications, TemplateHaskell, ViewPatterns #-}

module T08 where

import Utils
import Indexable

import Prelude.Unicode
import Data.Char
import Data.Vector ( Vector, fromList, (//) )
import Data.Function ( (&) )

import Control.Monad
import Control.Lens ( makeLenses, (%~), view )

data Instr = Nop Integer
           | Acc Integer
           | Jmp Integer
           deriving ( Show, Read )

data Machine = Machine { _instrs ∷ Vector Instr, _ip ∷ Integer, _accum ∷ Integer }
               deriving ( Show )
makeLenses ''Machine

initMachine ∷ Vector Instr → Machine
initMachine is = Machine { _instrs = is, _ip = 0, _accum = 0 }

step ∷ Machine → Either Integer Machine
step mach
  | ipcur ≥ fromIntegral (length (view instrs mach)) = Left ipcur
  | otherwise = Right $ mach & case view instrs mach ! fromInteger ipcur of
                    Nop _ → next
                    Acc v → next . (accum %~ (+ v))
                    Jmp v → ip %~ (+ v)
  where
    ipcur = view ip mach
    next = ip %~ (+ 1)

data Status = Cycled | Terminated deriving Show

runUntilCycleOrEnd ∷ Machine → (Status, Machine)
runUntilCycleOrEnd = go []
  where
    go seen mach@(step → Right next@(view ip → nip))
      | nip ∈ seen = (Cycled, mach)
      | otherwise  = go (nip:seen) next
    go _ mach = (Terminated, mach)

parseCode ∷ String → Vector Instr
parseCode = fromList . fmap parseInst . lines
  where
    parseInst = read @Instr . unwords . htmap (htmap toUpper id) fixSign . words
    fixSign xs@('-':_) = embrace xs
    fixSign ('+':xs)   = xs
    fixSign xs = xs

fixAt ∷ Vector Instr → Int → Maybe (Vector Instr)
fixAt vec idx = case vec ! idx of
    Nop x → Just $ vec // [(idx, Jmp x)]
    Jmp x → Just $ vec // [(idx, Nop x)]
    _ → Nothing

main ∷ IO ()
main = do
    code <- parseCode <$> getContents
    print . view accum . snd . runUntilCycleOrEnd . initMachine $ code
    let fixed = [ res | at ← [0 .. (length code - 1)]
                      , (Terminated, res) ← mayToList $ runUntilCycleOrEnd . initMachine <$> fixAt code at
                      ]
    print $ view accum (head fixed)
