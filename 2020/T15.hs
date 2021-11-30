{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables, BangPatterns, TypeApplications, LambdaCase #-}

module T15 where

import Indexable
import Utils

import Prelude.Unicode

import Data.Map ( Map, empty, insertWith )

import Control.Monad.State.Lazy ( evalState, State )
import Control.Lens

nums ∷ [Int] → [Int]
nums initial = evalState (start initial >> sequence (map pure initial ++ repeat gen)) (0, 0, empty)
  where
    start ∷ [Int] → State (Int, Int, Map Int [Int]) ()
    start []     = pure ()
    start (x:xs) = _1 <+= 1 >>= \round →
                   _3 %= insertWith merge x [round] >>
                   _2 .= x >>
                   start xs                      
    
    gen ∷ State (Int, Int, Map Int [Int]) Int
    gen = _1 <+= 1 >>= \round →
          use _2 >>= \lst →
          use (_3 . at lst) >>= \case
              Just [_]       → _3 %= insertWith merge 0 [round] >> _2 <.= 0
              Just (r0:r1:_) → let new = r0 - r1
                               in _3 %= insertWith merge new [round] >> _2 <.= new
              _ → error "broken map"
    merge a b = take 2 $ a ++ b

main ∷ IO ()
main = do
    input ← map (read @Int) . splitsBy (≡ ',') <$> getContents
    print $ nums input ! (2020 - 1)
    print $ nums input ! (30000000 - 1)
