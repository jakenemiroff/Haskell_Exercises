-- Author : JAKE NEMIROFF
-- Program to implement the insertion sort algorithm --

insert x [] = [x]	-- function insert will take a number x, and a sorted list, and will insert x into the proper location of the sorted list

insert x (l:ls)
  | x < l = x:l:ls
  | x > l = l : insert x ls
  | x == l = l : insert x ls

isort [] = []

isort (l:ls) = insert l (isort ls)	-- function isort uses recursion and the insert function to return a sorted list of numbers in ascending order
