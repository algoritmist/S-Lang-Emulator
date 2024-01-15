gcd(x, y) = If x == 0 Then y Else gcd(y % x, x);
lcm(x, y) = x * y / gcd(x, y);
findDiv(acc, l, r) = If l == r Then acc Else findDiv(lcm(acc, l), l + 1, r);
main() = outputInt(findDiv(1, 1, 21));