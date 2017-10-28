recursive :: Int -> Int
recursive n = if n <= 1 then n
	else if n==2 then 1
	else if n==3 then 2
	else recursive (n-1) + recursive(n-2) +recursive (n-3) + recursive (n-4)
--4**n	