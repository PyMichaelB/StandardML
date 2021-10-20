datatype 'a tree = Twig of 'a | Br of 'a *'a tree * 'a tree;
(*a*)

fun add(x, []) = []
	| add(x, (l::ls)) = (x::l)::add(x, ls);
	
fun paths (Twig(a)) = [[a]]
	| paths (Br(v, t1, t2)) = add(v, paths(t1)@paths(t2));

fun sum_of_list [] = 0
		| sum_of_list(y::ys) = y+sum_of_list(ys)
	
fun find_path p t = 
	let fun computePaths [] = [] 
	| computePaths(x::xs) = if p (sum_of_list(x)) then x else computePaths(xs)
	in computePaths(paths(t))
	end;
	
(*b*)
fun all_paths p t =
	let fun computePaths [] = []
	| computePaths (x::xs) = if p (sum_of_list(x)) then x::computePaths(xs) else computePaths(xs)
	in computePaths(paths(t))
	end;