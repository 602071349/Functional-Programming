tec :: Int -> Int
tec n = if n==0 then (addsum 0) !! 3
	else if n==1 then (addsum 1) !! 2
	else if n==2 then (addsum 2) !! 1
	else if n==3 then (addsum 3) !! 0
	else (addsum n) !! 0
addsum n = addsum1 [2,1,1,0] (n-3)
addsum1 lst n = if n<=0 then lst
	else addsum1 ([lst !! 0 + lst !! 1 + lst !! 2 + lst !! 3] ++ lst) (n-1)