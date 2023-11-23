-- This is an implementation of prob5. More information can be found by the link https://projecteuler.net/problem=5

divisible (x, l, r) = if l == r then True else x mod l == 0 and divisible(x, l+1, r);
findDiv (x, l, r) = if divisible(x, l, r) then x else findDiv(x + 1, l, r);
main = findDiv 1 1 20;
