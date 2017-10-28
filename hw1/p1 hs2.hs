
recursivefib n = if n <= 1 then n
    else  (recursivefib (n-1)) + (recursivefib (n-2))
fib 0 = "Please enter a positive integer"
fib nterms = if nterms == 1 then "0"
    else (fib (nterms-1)) ++" " ++ show((recursivefib (nterms-1)))
