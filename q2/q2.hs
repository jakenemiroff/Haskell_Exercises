-- Author : JAKE NEMIROFF
-- Program to compute the power a^n given two parameters 'a' and 'n' --

pow a n 
  | n == 0 = 1
  | n > 0 = a * pow a (n - 1)
