fun smallest [] = raise Empty
	| smallest[x] = x
	| smallest(x::xs) = if x < (smallest xs) 
						then x 
						else (smallest xs);
						
fun remove([], a) = []
	| remove(b::bs, a) = if b = a then bs
						else b::remove(bs, a);
						
fun selection ([]) = []
	| selection([y]) = [y]
	| selection(y::ys) = smallest(y::ys)::selection(remove(y::ys, smallest(y::ys)));
	
