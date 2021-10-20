fun nth([], n) = raise Empty
	| nth(b::ys, 0) = b
	| nth(a::xs, n) = nth(xs, n-1);
	
(*This function finds the nth element of a list. To find the even elements we 
need to take the 1st, 3rd, 5th etc. elements and conses them into a new list*)
fun even(n) = (n mod 2 = 0);
fun addLen(n, []) = n
	| addLen(n, x::xs) = addLen(n+1, xs);
	
fun evenEle([], newList, current) = newList
	| evenEle(x::xs, newList, current) = 
	if (even(addLen(0, x::xs))) then 
		if (current > addLen(0, x::xs)) then newList 
		else evenEle(x::xs, newList @ [nth(x::xs, current)], current+2)
	else 
		if (current = (addLen(0, x::xs))) then newList 
		else evenEle(x::xs, newList @ [nth(x::xs, current)], current+2);
	