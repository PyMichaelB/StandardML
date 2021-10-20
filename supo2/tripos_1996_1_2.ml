datatype BOOL = VAR of string | NOT of BOOL | AND of BOOL*BOOL | OR of BOOL*BOOL;
(*a*)

fun isIn(x, []) = false
	|   isIn(x, b::ys) =
		if x = b then true
		else isIn(x, ys);
		
fun union([], y) = y
	|	union(a::xs, y) = 
		if isIn(a, y) then union(xs, y)
		else a::union(xs, y);
	
fun bools(ls, VAR(x)) = union(ls, [x]) (*Union because we want distinct names*)
	| bools(ls, NOT(x)) = bools(ls, x)
	| bools(ls, AND(x,y)) = union(bools(ls, x), bools(ls, y))
	| bools(ls, OR(x, y)) = union(bools(ls, x), bools(ls, y));
	
fun names(exp) = bools([], exp); 

(*b*)
fun contexts(exp, contxt) =
	let fun eval(VAR(x),contxt) = isIn("x", contxt)
		| eval(NOT(x), contxt) = not (eval(x, contxt))
		| eval(AND(x, y), contxt) = eval(x, contxt) andalso eval(y, contxt)
		| eval(OR(x, y), contxt) =  eval(x, contxt) orelse eval(y, contxt)
	in eval(exp: BOOL, contxt: string list)
end;

(*c*)
(*We need all permutations of context*)
fun permutations [] = []
	| permutations(x::xs) =  
(*First, we find the names in our expression*)
(*Now, compute the first permutation (sort of permutations
- we want all combinations of our names being true or false
- and see if true. If true, then compute second else return false*)
exception No_Names;
fun compute(_, []) = raise No_Names
	| compute(exp, x::xs) = 
	if contexts(exp, x) then compute(exp, xs) else false;
	
fun is_true(exp) = compute(exp, permutations(names(exp), []));

