(*c*)
datatype 'a tree = Twig of 'a | Br of 'a *'a tree * 'a tree;
datatype 'a stream = Cons of 'a * (unit -> 'a stream);
fun add(x, []) = []
	| add(x, (l::ls)) = (x::l)::add(x, ls);
	
fun paths (Twig(a)) = [[a]]
	| paths (Br(v, t1, t2)) = add(v, paths(t1)@paths(t2));

fun sum_of_list [] = 0
		| sum_of_list(y::ys) = y+sum_of_list(ys)

fun all_pathsq p t =
	let fun computePaths [] = []
	| computePaths(x::xs) = if p (sum_of_list(x)) then x::computePaths(xs) else computePaths(xs)
	in computePaths(paths(t))
end;

fun nth (x::xs) 1 = x
	| nth (x::xs) n = nth xs (n-1);

fun tail (Cons(_, xf)) = xf();
exception No_More_Paths;

fun next (n: int) p t = 
let val l = length(all_pathsq p t)
fun strea_m n p path = Cons(nth (path) n, fn() => (if n<l then strea_m (n+1) p path else raise No_More_Paths))
in strea_m n p (all_pathsq p t)
end;
