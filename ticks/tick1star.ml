(*Terms not cheaply computed here*)
fun nthterm(1) = 1.0
	| nthterm(n)  = nthterm(n-1)/real(n-1);

fun eapprox(0) = 0.0
	| eapprox(n) = nthterm(n)+eapprox(n-1);

fun power(x, 0) = 1.0
	| power(x, n) = x*power(x, n-1)

fun exp(z, 0) = 0.0
	| exp(z, n) = (nthterm(n)*power(z, (n-1)))+exp(z,n-1);

fun gcd(1, b) = 1
	| gcd(a, 1) = 1
	| gcd(a, b) = 
		if a=b then a
		else if (a mod 2 = 0) andalso (b mod 2 = 0) then 2*gcd(a div 2, b div 2)
		else if (a mod 2 = 1) andalso (b mod 2 = 0) then gcd(a, b div 2)
		else if (b mod 2 = 1) andalso (a mod 2 = 0) then gcd(a div 2, b)
		else gcd(b, (a-b) div 2)