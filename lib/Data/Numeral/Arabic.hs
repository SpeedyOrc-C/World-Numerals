{-# LANGUAGE LambdaCase #-}

module Data.Numeral.Arabic (
    toEasternArabic,
    toPersian,
    toUrdu,
) where

toEasternArabic :: (Integral a, Show a) => a -> String
toEasternArabic = showEasternArabic . toArabicNumber

toPersian :: (Integral a, Show a) => a -> String
toPersian = showPersian . toArabicNumber

toUrdu :: (Integral a, Show a) => a -> String
toUrdu = toPersian

toArabicNumber :: (Integral a, Show a) => a -> ArabicNumber
toArabicNumber n = ArabicNumber (n < 0) (map toArabicNumeral $ show $ abs n)

toArabicNumeral :: Char -> ArabicNumeral
toArabicNumeral = \case
    '0' -> Ar0
    '1' -> Ar1; '2' -> Ar2; '3' -> Ar3
    '4' -> Ar4; '5' -> Ar5; '6' -> Ar6
    '7' -> Ar7; '8' -> Ar8; '9' -> Ar9
    _ -> error "Invalid numeral."

data ArabicNumeral
    = Ar0 | Ar1 | Ar2 | Ar3 | Ar4 | Ar5
    | Ar6 | Ar7 | Ar8 | Ar9
    deriving (Show, Eq)

class ShowEasternArabic a where
    showEasternArabic :: a -> String

class ShowPersian a where
    showPersian :: a -> String

data ArabicNumber = ArabicNumber Bool [ArabicNumeral]

instance ShowEasternArabic ArabicNumber where
    showEasternArabic :: ArabicNumber -> String
    showEasternArabic (ArabicNumber isNegative xs) = 
        concatMap showEasternArabic xs ++ if isNegative then "-" else ""

instance ShowPersian ArabicNumber where
    showPersian :: ArabicNumber -> String
    showPersian (ArabicNumber isNegative xs) =
        concatMap showPersian xs ++ if isNegative then "-" else ""

instance ShowEasternArabic ArabicNumeral where
    showEasternArabic :: ArabicNumeral -> String
    showEasternArabic = \case
        Ar0 -> "٠"
        Ar1 -> "١"; Ar2 -> "٢"; Ar3 -> "٣"
        Ar4 -> "٤"; Ar5 -> "٥"; Ar6 -> "٦"
        Ar7 -> "٧"; Ar8 -> "٨"; Ar9 -> "٩"

instance ShowPersian ArabicNumeral where
    showPersian :: ArabicNumeral -> String
    showPersian = \case
        Ar0 -> "۰"
        Ar1 -> "۱"; Ar2 -> "۲"; Ar3 -> "۳"
        Ar4 -> "۴"; Ar5 -> "۵"; Ar6 -> "۶"
        Ar7 -> "۷"; Ar8 -> "۸"; Ar9 -> "۹"
