recurse first last arr elem= if first > last then "is not in the array"
		else if arr !! (div (first+last) 2) < elem then  recurse (div (first+last) 2 + 1) last arr elem
		else if arr !! (div (first+last) 2) > elem then  recurse first (div (first+last) 2 - 1) arr elem
		else show (div (first+last) 2)
binsearch arr elem = recurse 0 (length arr - 1) arr elem
