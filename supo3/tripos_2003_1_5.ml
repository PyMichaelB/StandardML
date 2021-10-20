(*a*) 
datatype 'a stream = Cons of 'a * (unit -> 'a stream);
(*Lazy lists can be implemented by using streams or sequences.
Using the datatype declaration above we can define lazy lists 
by Cons(x, xf) where x is the first element and xf 
represents the rest of the list. Calling xf() generates the next 
element - it is called the tail function.*)

(*b*)
fun concat(Nil, ys) = ys
	| concat (Cons(x, xf), ys) = Cons(x, fn() => concat(xf(), ys));

(*If the function is applied to a pair of infinite lists, the
result will just be the first list because we will never get 
round to the second list*)

(*c*)
fun interleave(Nil, ys) = ys
	| interleave (Cons(x, xf), ys) = Cons(x, fn() => interleave(ys, xf()));

(*d*)
(*This part computes the list of lists which have a length n 
and then puts them into the lazy list*)
fun list_of_1_0 0 = []
	| list_of_1_0 1 = [[0], [1]]
	| list_of_1_0 n = (map(fn x => 0::x)(list_of_1_0(n-1))) @ (map(fn x => 1::x)(list_of_1_0(n-1)));

fun nth [] _ = raise Empty
	| nth (x::xs) 0 = x
	| nth (x::xs) n = nth xs (n-1);

fun o1sStream (n:int) k = 
	let val xs = list_of_1_0 n
		fun strem n k xs = Cons(nth xs k, fn() => if (k+1)>=length(xs) then o1sStream (n+1) 0 else o1sStream n (k+1))
in strem n k xs
end;

(*e*)
fun o1sStreamPalin (n:int) k =
	let val xs = list_of_1_0 n
	fun palins [] ls = ls
		| palins (x::xs) ls = if rev(x) = x then palins xs (x::ls) else palins xs ls
		fun strem n k xs = Cons(nth xs k, fn() => if (k+1)>=length(xs) then o1sStreamPalin (n+1) 0 else o1sStreamPalin n (k+1))
	in strem n k (palins xs [])
end;



