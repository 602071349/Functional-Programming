module Mandelbrot where

prefix n xs = if xs==[] then xs
	else if n==0 then []
	else [xs!!0] ++ prefix (n-1) (tail xs)
	
suffix n xs = if xs==[] then xs
	else if n==0 then xs
	else suffix (n-1) (tail xs)

split n y xs
 | (suffix n xs) ==[] = xs
 | otherwise = (prefix n xs) ++ (y : (split n y (suffix n xs)))

--split n y xs =if (length xs)>n then (prefix n xs) ++ [y] ++ (split n y (suffix n xs))	
--plane r =if r>0 then [(x*1.0,y*1.0)|y<-[(-r)..r],x<-[(-2)*r..r]]
	--else [(x*1.0,y*1.0)|y<-[r..(-r)],x<-[r..(-2)*r]]

plane r = [((x/r),(y/r)) | y<- [(-r).. r], x <- [(-r*2.0).. (r*1.0)]]

orbit (x,y) =(0,0):[p (x,y) ((fst i),(snd i))|i <- (orbit (x,y))]
	where
	p (x,y) (u,v) =(u*u-v*v+x,2*u*v+y)

disp d [] = ' '
disp  d (x:xs) = if (fst x)>d then snd x
	else disp d xs

norm (x,y) =x*x + y*y

mandelbrot r i l=split (3*r+1) '\n' ([disp c l |c <-[norm ((fst x),(snd x)) | x<-[(orbit ((fst q),(snd q)))!!i | q<-(plane r) ]]])

