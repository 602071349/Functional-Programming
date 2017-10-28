sumlst k lst= if (length lst) < k then sum lst
	else sum (take k lst)
addsum1 lst n k = if n<= 0 then lst
	else addsum1 ([sumlst k lst] ++ lst) (n-1) k
addsum n k = addsum1 [1,1,0] (n-2) k
tec :: Int -> Int -> Int
tec n k = if n==0 then (addsum 0 k) !! 2
	else if n==1 then (addsum 0 k) !! 1
	else if n==2 then (addsum 0 k) !! 0
	else (addsum n k) !! 0