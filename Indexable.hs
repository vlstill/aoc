{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables, MultiParamTypeClasses,
             TypeFamilies, FlexibleInstances, FlexibleContexts,
             UndecidableInstances #-}

module Indexable where

import Data.Kind ( Type )

import Data.Vector ( Vector )
import qualified Data.Vector ( (!?) )

class Indexable α ι where
    type Indexed α ι ∷ Type
    (!) ∷ α → ι → Indexed α ι
    (!?) ∷ α → ι → Maybe (Indexed α ι)

    cont ! i = case cont !? i of
                  Nothing → error "Index out of range"
                  Just x  → x

instance Indexable [α] Int where
    type Indexed [α] Int = α
    
    []     !? _ = Nothing
    (x:_)  !? 0 = Just x
    (_:xs) !? n = xs !? (n - 1)

instance Indexable (Vector α) Int where
    type Indexed (Vector α) Int = α

    (!?) = (Data.Vector.!?)

instance (Indexable α ι, Indexable (Indexed α ι) γ) ⇒ Indexable α (ι, γ) where
    type Indexed α (ι, γ) = Indexed (Indexed α ι) γ

    cont !? (i, j) = (cont !? i) >>= (!? j)
