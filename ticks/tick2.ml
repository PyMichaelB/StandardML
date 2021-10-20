fun last [] = raise Empty
	| last [x]= x
	| last (_::xs) = last xs;
	
fun butlast ([]) = raise Empty
	| butlast ([x]) = nil
	| butlast (a::xs) = a::butlast(xs);
	
fun nth([], n) = raise Empty
	| nth(b::ys, 0) = b
	| nth(a::xs, n) = nth(xs, n-1);
	