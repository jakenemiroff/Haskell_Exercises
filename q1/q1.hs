-- Author : JAKE NEMIROFF
-- Program to compute the multiplication of two given values, a and b --

mult a b 
  | b == 0 = 0
  | b > 0 = a + a * (b-1)
