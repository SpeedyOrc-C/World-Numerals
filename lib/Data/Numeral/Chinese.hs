{-# LANGUAGE LambdaCase #-}

module Data.Numeral.Chinese (
    toChineseNormal, 转中文普通数字,
    toChineseFinancial, 转中文金融数字,
    toChineseCasual, 转中文口语数字,
) where

toChineseNormal :: Integer -> String
toChineseNormal = showNormalChinese . toChineseNumberNormal
转中文普通数字 :: Integer -> String
转中文普通数字 = toChineseNormal

toChineseFinancial :: Integer -> String
toChineseFinancial = showFinancialChinese . toChineseNumberNormal
转中文金融数字 :: Integer -> String
转中文金融数字 = toChineseFinancial

toChineseCasual :: Integer -> String
toChineseCasual = showNormalChinese . toChineseNumberCasual
转中文口语数字 :: Integer -> String
转中文口语数字 = toChineseCasual

class ShowNormalChinese a where
    showNormalChinese :: a -> String

class ShowFinancialChinese a where
    showFinancialChinese :: a -> String

toChineseNumeral :: Integer -> ChineseNumeral
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
    showNormalChinese Cn0 = "零"
    showNormalChinese Cn1 = "一"
    showNormalChinese Cn2 = "二"
    showNormalChinese Cn2Liang = "两"
    showNormalChinese Cn3 = "三"
    showNormalChinese Cn4 = "四"
    showNormalChinese Cn5 = "五"
    showNormalChinese Cn6 = "六"
    showNormalChinese Cn7 = "七"
    showNormalChinese Cn8 = "八"
    showNormalChinese Cn9 = "九"
    showNormalChinese Cn10 = "十"
    showNormalChinese Cn100 = "百"
    showNormalChinese Cn1000 = "千"
    showNormalChinese Cn10000 = "万"
    showNormalChinese Cn100000000 = "亿"

instance ShowFinancialChinese ChineseNumeral where
    showFinancialChinese :: ChineseNumeral -> String
    showFinancialChinese Cn0 = "零"
    showFinancialChinese Cn1 = "壹"
    showFinancialChinese Cn2 = "贰"
    showFinancialChinese Cn2Liang = "贰"
    showFinancialChinese Cn3 = "叁"
    showFinancialChinese Cn4 = "肆"
    showFinancialChinese Cn5 = "伍"
    showFinancialChinese Cn6 = "陆"
    showFinancialChinese Cn7 = "柒"
    showFinancialChinese Cn8 = "捌"
    showFinancialChinese Cn9 = "玖"
    showFinancialChinese Cn10 = "拾"
    showFinancialChinese Cn100 = "佰"
    showFinancialChinese Cn1000 = "仟"
    showFinancialChinese Cn10000 = "萬"
    showFinancialChinese Cn100000000 = "億"

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

toChineseNumberNormal :: Integer -> ChineseNumber
toChineseNumberNormal = ChineseNumber . toChineseNumeralsNormal

toChineseNumeralsNormal :: Integer -> [ChineseNumeral]
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

toChineseNumberCasual :: Integer -> ChineseNumber
toChineseNumberCasual = ChineseNumber . toChineseNumeralsCasual

toChineseNumeralsCasual :: Integer -> [ChineseNumeral]
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
