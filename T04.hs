{-# LANGUAGE UnicodeSyntax, ViewPatterns, ScopedTypeVariables, MultiParamTypeClasses, TypeFamilies, LambdaCase, TypeApplications #-}

module T03 where

import Prelude.Unicode
import Data.Monoid
import Data.List ( unfoldr, (\\) )
import Data.Maybe
import Control.Arrow
import Text.Read

data Field = BirthYear
           | IssueYear
           | ExpirationYear
           | Height
           | HairColor
           | EyeColor
           | PassportID
           | CountryID
           deriving ( Show, Eq, Enum )

parseField ∷ String → Field
parseField "byr" = BirthYear
parseField "iyr" = IssueYear
parseField "eyr" = ExpirationYear
parseField "hgt" = Height
parseField "hcl" = HairColor
parseField "ecl" = EyeColor
parseField "pid" = PassportID
parseField "cid" = CountryID
parseField fi    = error $ "Invalid field " <> fi

type Pass = [(Field, String)]


splitsBy ∷ ∀α. (α → Bool) → [α] → [[α]]
splitsBy p = unfoldr (\case
                        [] → Nothing
                        xs → Just . second (drop 1) $ break p xs)

parsePassEntry ∷ String → (Field, String)
parsePassEntry (break (≡ ':') → (k, ':':v)) = (parseField k, v)
parsePassEntry ent = error $ "Invid pass entry " <> ent

valid ∷ Pass → Bool
valid (fmap fst → fields) = null $ [BirthYear .. PassportID] \\ fields

validFields ∷ Pass → Bool
validFields = all (uncurry check)
  where
    check BirthYear (readMaybe → Just y)        = 1920 ≤ y ∧ y ≤ 2002
    check IssueYear (readMaybe → Just y)        = 2010 ≤ y ∧ y ≤ 2020
    check ExpirationYear (readMaybe → Just y)   = 2020 ≤ y ∧ y ≤ 2030
    check Height (reads @Integer → [(hgt, un)]) = un ≡ "cm" ∧ 150 ≤ hgt ∧ hgt ≤ 193
                                                ∨ un ≡ "in" ∧  59 ≤ hgt ∧ hgt ≤  76
    check HairColor ('#':col@(length → 6))      = isJust (readMaybe @Integer ("0x" <> col))
    check EyeColor  col                         = any (≡ col) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    check PassportID pid@(length → 9)           = isJust (readMaybe @Integer pid)
    check CountryID  _                          = True
    check _ _                                   = False


countWith ∷ ∀τ α. Foldable τ ⇒ (α → Bool) → τ α → Int
countWith proj = getSum . foldMap (Sum . fromEnum . proj)

main ∷ IO ()
main = do
    pass ← fmap (fmap parsePassEntry . words . unwords) . splitsBy (≡ []) . lines <$> getContents
    print $ countWith valid pass
    print $ countWith ((∧) <$> valid <*> validFields) pass
