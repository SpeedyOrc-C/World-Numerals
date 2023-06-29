{-# LANGUAGE LambdaCase #-}

module Data.Numeral.Roman where

toRoman :: Integral a => a -> String
toRoman = showRoman . RomanNumber . toRomanNumerals

data RomanNumeral
    = Rm1 | Rm5 | Rm10 | Rm50 | Rm100 | Rm500 | Rm1000
    deriving (Eq, Show)

data RomanNumber = RomanNumber [RomanNumeral] deriving (Eq, Show)

class ShowRoman a where
    showRoman :: a -> String

instance ShowRoman RomanNumeral where
    showRoman :: RomanNumeral -> String
    showRoman Rm1 = "I"
    showRoman Rm5 = "V"
    showRoman Rm10 = "X"
    showRoman Rm50 = "L"
    showRoman Rm100 = "C"
    showRoman Rm500 = "D"
    showRoman Rm1000 = "M"

instance ShowRoman RomanNumber where
    showRoman :: RomanNumber -> String
    showRoman (RomanNumber xs) = concatMap showRoman xs

toRomanNumerals :: Integral a => a -> [RomanNumeral]
toRomanNumerals n
    | 0 <= n && n <= 3 = replicate (fromIntegral n) Rm1
    | n == 4 = [Rm1, Rm5]
    | n == 5 = [Rm5]
    | n <= 8 = Rm5 : toRomanNumerals (n - 5)
    | n == 9 = [Rm1, Rm10]
    | n <= 39 = 
        replicate (fromIntegral (n `div` 10)) Rm10
        ++ toRomanNumerals (n `mod` 10)
    | n <= 49 = [Rm10, Rm50] ++ toRomanNumerals (n - 40)
    | n <= 89 = Rm50 : toRomanNumerals (n - 50)
    | n <= 99 = [Rm10, Rm100] ++ toRomanNumerals (n - 90)
    | n <= 399 = 
        replicate (fromIntegral (n `div` 100)) Rm100
        ++ toRomanNumerals (n `mod` 100)
    | n <= 499 = [Rm100, Rm500] ++ toRomanNumerals (n - 400)
    | n <= 899 = Rm500 : toRomanNumerals (n - 500)
    | n <= 999 = [Rm100, Rm1000] ++ toRomanNumerals (n - 900)
    | n <= 3999 = 
        replicate (fromIntegral (n `div` 1000)) Rm1000
        ++ toRomanNumerals (n `mod` 1000)
    | otherwise = error "toRomanNumerals: number too large"
