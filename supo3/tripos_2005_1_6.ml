(*c*)
datatype 'a tree = Twig of 'a | Br of 'a *'a tree * 'a tree;
datatype 'a stream = Cons of 'a * (unit -> 'a stream);
fun add(x, []) = []
	| add(x, (l::ls)) = (x::l)::add(x, ls);
	
fun paths (Twig(a)) = [[a]]
	| paths (Br(v, t1, t2)) = add(v, paths(t1)@paths(t2));

fun sum_of_list [] = 0
		| sum_of_list(y::ys) = y+sum_of_list(ys)

fun satisfying_paths p t =
	let fun computePaths [] = []
	| computePaths(x::xs) = if p (sum_of_list(x)) then x::computePaths(xs) else computePaths(xs)
	in computePaths(paths(t))
end;

exception No_More_Paths;

fun all_pathsq p t = 
let fun strea_m p [] = raise No_More_Paths
	| strea_m p (x::xs) = Cons(x, fn() => strea_m p xs)
	in strea_m p (satisfying_paths p t)
end;

