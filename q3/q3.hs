-- Author : JAKE NEMIROFF
-- Program to compute the power a^n given two parameters 'a' and 'n' using pattern matching--

pow' _ 0 = 1 

pow' 0 _ = 0

pow' a n = a * pow' a (n - 1) 
