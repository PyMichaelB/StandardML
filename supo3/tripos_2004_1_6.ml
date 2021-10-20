(*Using the notation fn x => x+1 for example, we have a function 
hich takes an integer x and returns the x plus one. It does not 
ave a name and fn is used to state that this is a function. 
The arrow => is used to say, given the thing on the left hand 
side, do the thing on the right hand side.*)
datatype 'a tree = Lf | Br of 'a *'a tree * 'a tree;

fun tcons (v, Lf) = Br (v, Lf, Lf)
      | tcons (v, (Br (w, t1, t2))) = Br (v, tcons (w, t2), t1);

(*Creates array from a list*)
fun create_array_from_list [] = Lf
	|	create_array_from_list(x::xs) = tcons(x, create_array_from_list(xs));
exception Leaf
fun retrieve (Lf, _) = raise Leaf
	| retrieve(Br(v, t1, t2), k) =
	if k = 1 then v
	else if k mod 2 = 0 then retrieve(t1, k div 2)
		else retrieve(t2, k div 2);

fun update(Lf, k, v1) = raise Leaf
	| update(Br(v, t1, t2), k, v1) = 
	if k = 1 then Br(v1, t1, t2)
	else if k mod 2 = 0 then Br(v, update(t1, k div 2, v1), t2)
		else Br(v, t1, update(t2, k div 2, v1));

(*Could we discuss how to calculate the cost in terms of big O notation*)

(*b*) 
(*int stream.*)
datatype 'a stream = Cons of 'a * (unit -> 'a stream) | Nil;
fun tail (Cons(_, xf)) = xf();

fun map_lazy_list f Nil = Nil
	| map_lazy_list f (Cons(x, xf))
	= Cons(f x, fn() => map_lazy_list f (xf()) );

exception No_Sub;
fun getSub (Nil, _, _) = raise No_Sub
	| getSub(Cons(x, xf), i, n) = if x=i then n else getSub(xf(), i, n+1);

fun squares k = Cons(k*k, fn()=>squares(k+1));
fun updateStream a n v = map_lazy_list (fn x=> if getSub(a, x, 0) = n-1 then v else x)(a);

(*I think this update function is O(n^2) in the worst case where n is the index because you have to find the subscript (by going up the lazylist and then apply that to every element. After a sequence of updates the cost of accessing the array is the same as before the updates?*)
