-- Author : JAKE NEMIROFF
-- Program to convert a given string into l33t-speak --

import Data.Char(ord, chr)	--import ord and chr functions

isVowel l = l `elem` "aAoOuU"	--helper function to determine whether a character is a vowel

isConsonant l = l `elem` "bBcCdDfFgGhHjJkKlLmMnNpPqQrRsStTvVwWxXyYzZ"	--helper function to determine whether a character is a consonant

isUpper l = l `elem` "ABCDFGHJKLMNOPQRSTUVWXYZ" --helper function to determine whether a character is uppercase

isLower l = l `elem` "abcdfghjklmnopqrstuvwxy" --helper function to determine whether a character is lowercase

toLower l = chr (ord l + 32) --helper function to convert an uppercase character to lowercase

toUpper l = chr (ord l - 32) --helper function to convert an lowercase character to uppercase

letterI l = l `elem` "iI" --helper function to determine whether a character is the letter 'i'

letterE l = l `elem` "eE" --helper function to determine whether a character is the letter 'e'

charIsExclamation p = p `elem` "!" --helper function to determine whether a character is a '!'

l33t [] = []

l33t (x:xs) --l33t function will convert characters from a given string to the desired output based on these guarded statements
  | letterE x == True = '3' : l33t xs
  | letterI x == True = '1' : l33t xs
  | charIsExclamation x == True = "!!!111oneone" ++ l33t xs
  | isVowel x && isUpper x == True = toLower x : l33t xs
  | isVowel x && not (isUpper x) == True = x : l33t xs
  | isConsonant x && isUpper x == True = x : l33t xs
  | isConsonant x && isLower x == True = toUpper x : l33t xs
  | otherwise = x : l33t xs 
