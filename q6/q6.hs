-- Author : JAKE NEMIROFF
-- Program to implement the merge sort algorithm --

merge a b		-- function merge will recursively merge two lists of any size
  | length a == 0 = b
  | length b == 0 = a


merge (a:as) (b:bs)
  | a < b = a : merge as (b:bs)
  | otherwise = b : merge (a:as) bs

msort a
  | length a < 2 = a

msort a = merge (msort firstHalf) (msort lastHalf)	-- function msort will recursively merge the lists by splitting them into halfs, sorting them, and then merging them back together
  where firstHalf = take (length a `div` 2) a
        lastHalf = drop (length a `div` 2) a
