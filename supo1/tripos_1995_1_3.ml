fun nth([], n) = raise Empty
	| nth(b::ys, 0) = b
	| nth(a::xs, n) = nth(xs, n-1);
		
fun isIn(x, []) = false
	|   isIn(x, b::ys) =
		if x = b then true
		else isIn(x, ys);

fun intersect([], y) = []
	|   intersect(a::xs, y) =
		if isIn(a, y) then a::intersect(xs, y)
		else intersect(xs, y);
		

fun subtract([], y) = []
	|   subtract(a::xs, y) =
		if isIn(a, y) then subtract(xs, y)
		else a::subtract(xs, y);

fun union([], y) = y
	|	union(a::xs, y) = 
		if isIn(a, y) then union(xs, y)
		else a::union(xs, y);
(* Union condition does not satisfy condition (d). Could we discuss this in the supervision? *)	
(* Most general type of union function: ''a list * ''a list -> ''a list*)	
	