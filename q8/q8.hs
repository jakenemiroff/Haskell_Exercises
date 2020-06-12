-- Author : JAKE NEMIROFF
-- Program to convert a given string of hexadecimal numbers to a decimal value --

import Data.Char (ord) -- import ord function

toInt c		--helper function in order to convert hexadecimal characters to integers
  | (48 <= (ord c) && (ord c) <= 57) = ord c - 48
  | c `elem` "aA" = 10
  | c `elem` "bB" = 11
  | c `elem` "cC" = 12
  | c `elem` "dD" = 13
  | c `elem` "eE" = 14
  | c `elem` "fF" = 15
  | otherwise = 16 --if the character is an invalid one, assign it a value of 16 (a number not in hex)

hexStrToDec [] = 0 

hexStrToDec (c:cs)	--function to convert the hexadecimal string to a decimal value
  | c == 'x' || c == 'X' = hexStrToDec cs
  | (toInt c < 16) = toInt c * 16 ^ (length (c:cs) - 1) + hexStrToDec cs
  | otherwise = error "Non-hexadecimal digits present"
