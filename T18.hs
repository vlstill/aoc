{-# LANGUAGE LambdaCase, UnicodeSyntax #-}

module T18 where

import Utils
import Data.List
import Language.Haskell.Interpreter

run ∷ String → IO Integer
run code = runInterpreter (setImports ["Prelude"] >> interpret code (1 :: Integer)) >>= \case
                    Right x → pure x
                    Left er → fail $ show er

subst ∷ Char → Char
subst '+' = '⊕'
subst '*' = '⊛'
subst x   = x

addDefs ∷ String → Int → String
addDefs code prioPlus = "let (⊛) = (*); infixl 8 ⊛; (⊕) = (+); infixl " ++ show prioPlus ++ " ⊕ in " ++ code

main ∷ IO ()
main = do
    expr ← intercalate " + " . map (embrace . map subst) . lines <$> getContents
    print =<< run (addDefs expr 8)
    print =<< run (addDefs expr 9)
