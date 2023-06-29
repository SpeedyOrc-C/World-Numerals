{-# LANGUAGE LambdaCase #-}

module Data.Numeral.Chinese (
    toChineseNormal, 转中文普通数字,
    toChineseFinancial, 转中文金融数字,
    toChineseCasual, 转中文口语数字,
) where

toChineseNormal :: Integral a => a -> String
toChineseNormal = showNormalChinese . toChineseNumberNormal
转中文普通数字 :: Integral a => a -> String
转中文普通数字 = toChineseNormal

toChineseFinancial :: Integral a => a -> String
toChineseFinancial = showFinancialChinese . toChineseNumberNormal
转中文金融数字 :: Integral a => a -> String
转中文金融数字 = toChineseFinancial

toChineseCasual :: Integral a => a -> String
toChineseCasual = showNormalChinese . toChineseNumberCasual
转中文口语数字 :: Integral a => a -> String
转中文口语数字 = toChineseCasual

class ShowNormalChinese a where
    showNormalChinese :: a -> String

class ShowFinancialChinese a where
    showFinancialChinese :: a -> String

toChineseNumeral :: Integral a => a -> ChineseNumeral
toChineseNumeral = \case
    0 -> Cn0; 1 -> Cn1; 2 -> Cn2; 3 -> Cn3; 4 -> Cn4; 5 -> Cn5
    6 -> Cn6; 7 -> Cn7; 8 -> Cn8; 9 -> Cn9; 10 -> Cn10
    100 -> Cn100; 1000 -> Cn1000; 10000 -> Cn10000;
    100000000 -> Cn100000000
    _ -> error "Invalid numeral."

data ChineseNumeral
    = Cn0 | Cn1 | Cn2 | Cn2Liang | Cn3 | Cn4 | Cn5
    | Cn6 | Cn7 | Cn8 | Cn9 | Cn10
    | Cn100 | Cn1000 | Cn10000 | Cn100000000
    deriving (Show, Eq)

instance ShowNormalChinese ChineseNumeral where
    showNormalChinese :: ChineseNumeral -> String
    showNormalChinese = \case
        Cn0 -> "零"
        Cn1 -> "一"
        Cn2 -> "二"
        Cn2Liang -> "两"
        Cn3 -> "三"
        Cn4 -> "四"
        Cn5 -> "五"
        Cn6 -> "六"
        Cn7 -> "七"
        Cn8 -> "八"
        Cn9 -> "九"
        Cn10 -> "十"
        Cn100 -> "百"
        Cn1000 -> "千"
        Cn10000 -> "万"
        Cn100000000 -> "亿"

instance ShowFinancialChinese ChineseNumeral where
    showFinancialChinese :: ChineseNumeral -> String
    showFinancialChinese = \case
        Cn0 -> "零"
        Cn1 -> "壹"
        Cn2 -> "贰"
        Cn2Liang -> "贰"
        Cn3 -> "叁"
        Cn4 -> "肆"
        Cn5 -> "伍"
        Cn6 -> "陆"
        Cn7 -> "柒"
        Cn8 -> "捌"
        Cn9 -> "玖"
        Cn10 -> "拾"
        Cn100 -> "佰"
        Cn1000 -> "仟"
        Cn10000 -> "萬"
        Cn100000000 -> "億"

newtype ChineseNumber = ChineseNumber [ChineseNumeral] deriving (Show, Eq)

instance ShowNormalChinese ChineseNumber where
    showNormalChinese :: ChineseNumber -> String
    showNormalChinese (ChineseNumber xs) = concatMap showNormalChinese xs

instance ShowFinancialChinese ChineseNumber where
    showFinancialChinese :: ChineseNumber -> String
    showFinancialChinese (ChineseNumber xs) = concatMap showFinancialChinese xs

keepWhen :: Bool -> a -> [a]
keepWhen True x = [x]
keepWhen False _ = []

toChineseNumberNormal :: Integral a => a -> ChineseNumber
toChineseNumberNormal = ChineseNumber . toChineseNumeralsNormal

toChineseNumeralsNormal :: Integral a => a -> [ChineseNumeral]
toChineseNumeralsNormal n
    | 0 <= n && n <= 9 = [toChineseNumeral n]
    | 10 == n = [Cn1, Cn10]
    | 11 <= n && n <= 99 =
        let (q, r) = n `quotRem` 10 in
            [toChineseNumeral q, Cn10]
            ++ keepWhen (r /= 0) (toChineseNumeral r)

    | 100 == n = [Cn1, Cn100]
    | 101 <= n && n <= 999 =
        let (q, r) = n `quotRem` 100 in
            [toChineseNumeral q, Cn100]
            ++ keepWhen (r `div` 10 == 0) Cn0
            ++ toChineseNumeralsNormal r

    | 1000 == n = [Cn1, Cn1000]
    | 1001 <= n && n <= 9999 =
        let (q, r) = n `quotRem` 1000 in
            [toChineseNumeral q, Cn1000]
            ++ keepWhen (r `div` 100 == 0) Cn0
            ++ toChineseNumeralsNormal r

    | 10000 == n = [Cn1, Cn10000]
    | 100000 == n = [Cn1, Cn10, Cn10000]
    | 1000000 == n = [Cn1, Cn100, Cn10000]
    | 10000000 == n = [Cn1, Cn1000, Cn10000]
    | 10001 <= n && n <= 99999999 =
        let (q, r) = n `quotRem` 10000 in
            toChineseNumeralsNormal q ++ [Cn10000]
            ++ keepWhen (r `div` 1000 == 0) Cn0
            ++ toChineseNumeralsNormal r
    
    | 100000000 == n = [Cn1, Cn100000000]
    | 1000000000 == n = [Cn1, Cn10, Cn100000000]
    | 10000000000 == n = [Cn1, Cn100, Cn100000000]
    | 100000000000 == n = [Cn1, Cn1000, Cn100000000]
    | 100000001 <= n && n <= 999999999999 =
        let (q, r) = n `quotRem` 100000000 in
            toChineseNumeralsNormal q ++ [Cn100000000]
            ++ keepWhen (r `div` 10000 == 0) Cn0
            ++ toChineseNumeralsNormal r

    | otherwise = error "toChineseNumber': not implemented"

toChineseNumberCasual :: Integral a => a -> ChineseNumber
toChineseNumberCasual = ChineseNumber . toChineseNumeralsCasual

toChineseNumeralsCasual :: Integral a => a -> [ChineseNumeral]
toChineseNumeralsCasual n
    | 10 == n = [Cn10]
    | 11 <= n && n <= 19 =
        let r = n `rem` 10 in
            [Cn10, toChineseNumeral r]

    | 100 == n = [Cn1, Cn100]
    | 101 <= n && n <= 999 =
        let (q, r) = n `quotRem` 100 in
            [if q == 2 then Cn2Liang else toChineseNumeral q, Cn100]
            ++ if (r `div` 10 == 0)
                then Cn0 : toChineseNumeralsNormal r
                else toChineseNumeralsNormal r

    | 1000 == n = [Cn1, Cn1000]
    | 1001 <= n && n <= 9999 =
        let (q, r) = n `quotRem` 1000 in
            [if q == 2 then Cn2Liang else toChineseNumeral q, Cn1000]
            ++ if (r `div` 100 == 0)
                then Cn0 : toChineseNumeralsNormal r
                else toChineseNumeralsCasual r

    | 100000 == n = [Cn10, Cn10000]
    | 1000000 == n = [Cn1, Cn100, Cn10000]
    | 10000000 == n = [Cn1, Cn1000, Cn10000]
    | 10001 <= n && n <= 99999999 =
        let (q, r) = n `quotRem` 10000 in
            (if q == 2 then [Cn2Liang] else toChineseNumeralsCasual q)
            ++ [Cn10000]
            ++ if r == 0 then [] else
                if (r `div` 1000 == 0)
                then Cn0 : toChineseNumeralsNormal r
                else toChineseNumeralsCasual r
    
    | 1000000000 == n = [Cn10, Cn100000000]
    | 10000000000 == n = [Cn1, Cn100, Cn100000000]
    | 100000000000 == n = [Cn1, Cn1000, Cn100000000]
    | 100000001 <= n && n <= 999999999999 =
        let (q, r) = n `quotRem` 100000000 in
        (if q == 2 then [Cn2Liang] else toChineseNumeralsCasual q)
        ++ [Cn100000000]
        ++ if r == 0 then [] else
            if (r `div` 10000 == 0)
            then Cn0 : toChineseNumeralsNormal r
            else toChineseNumeralsCasual r

    | otherwise = toChineseNumeralsNormal n
