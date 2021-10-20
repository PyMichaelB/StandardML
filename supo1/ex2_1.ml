fun power(x, n, tot) = 
	if n=0 then tot
	else power(x, n-1, tot*x);