fun evalquad (a, b, c, x:real) = a*(x*x) + b*x + c;

fun facr(n) = 
	if n > 1 then n*facr(n-1)
	else 1;

fun other(n, total) =
	if n=0 then total
	else other(n-1, total*n);
fun faci(n) = other (n, 1);

fun halfpower (n, num:real) =
	if n=0 then num
	else halfpower(n-1, num/2.0);
	
fun geometric(n:int, sum:real) =
	if n = 0 then sum
	else geometric(n-1, sum + halfpower(n-1, 1.0));
	
fun sumt(n:int) = geometric(n, 0.0);
