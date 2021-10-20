(* Code snippet to compute merge sort on an array *)

fun merge([], xs) = xs
	| merge(ys, []) = ys
	| merge(x::xs, y::ys) = if x<=y then x::merge(xs, y::ys) else y::merge(x::xs, ys);

exception Empty;

fun take [] _ = []
	| take (x::xs) 0 = []
	| take (x::xs) n = x::take xs (n-1);

fun drop [] _ = []
	| drop ys 0 = ys
	| drop (x::xs) n = drop xs (n-1);

fun mergesort [] = []
	| mergesort [x] = [x]
	| mergesort l = 
	let val k = length l
in merge(mergesort(take l (k div 2)), mergesort(drop l (k div 2)))
end;