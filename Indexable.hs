{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables, MultiParamTypeClasses,
             TypeFamilies, FlexibleInstances, FlexibleContexts,
             UndecidableInstances #-}

module Indexable where

import Data.Kind ( Type )

import Data.Vector ( Vector )
import qualified Data.Vector ( (!?) )

import Data.Map ( Map )
import qualified Data.Map ( (!?) )

class Indexable α where
    type Index α ∷ Type
    type Indexed α ∷ Type
    (!) ∷ α → Index α → Indexed α
    (!?) ∷ α → Index α → Maybe (Indexed α)


    cont ! i = case cont !? i of
                  Nothing → error "Index out of range"
                  Just x  → x

(‽) ∷ Indexable α ⇒ α → Index α → Maybe (Indexed α)
(‽) = (!?)

instance Indexable [α] where
    type Index [α] = Int
    type Indexed [α] = α
    
    []     !? _ = Nothing
    (x:_)  !? 0 = Just x
    (_:xs) !? n = xs !? (n - 1)

instance Indexable (Vector α) where
    type Index (Vector α) = Int
    type Indexed (Vector α) = α

    (!?) = (Data.Vector.!?)

instance Ord κ ⇒ Indexable (Map κ α) where
    type Index (Map κ α) = κ
    type Indexed (Map κ α) = α

    (!?) = (Data.Map.!?)

{-
instance (Indexable α ι, Indexable (Indexed α ι) γ) ⇒ Indexable α (ι, γ) where
    type Indexed α (ι, γ) = Indexed (Indexed α ι) γ

    cont !? (i, j) = (cont !? i) >>= (!? j)
-}
