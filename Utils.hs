{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables, LambdaCase #-}

module Utils where

import Data.List ( unfoldr )
import Control.Arrow ( second )

splitsBy ∷ ∀α. (α → Bool) → [α] → [[α]]
splitsBy p = unfoldr (\case
                        [] → Nothing
                        xs → Just . second (drop 1) $ break p xs)
