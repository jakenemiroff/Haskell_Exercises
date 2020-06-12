-- Author : JAKE NEMIROFF
-- Functions to practice list comprehensions --

odds = [x * 2 - 1 | x <- [1..5]]	 -- odds will store a list of all odd numbers from 1 to 10

positiveEvens = [x * 2 | x <- [1..]] 	-- positiveEvens will store an infinite list of positive even numbers

powersOfTwo = [2 ^ x | x <- [0..]] 	-- powersOfTwo will store an infinite list of powers of two

isPrime 				--isPrime is a helper function to determine if a number is prime
  | x <= 1 = False
  | otherwise = null [k | k <- [2..x - 1], x `mod` k == 0]

firstNPrimes n = take n [x | x <- [2..], isPrime x] -- firstNPrimes builds an infinite list of prime numbers
