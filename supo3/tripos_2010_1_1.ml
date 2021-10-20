(*a*)
datatype 'a stream = Nil | Cons of 'a * (unit -> 'a stream);
(*b*)
fun interleave (Nil, ys)  = ys
	| interleave(Cons(x, xf), ys) = 
	Cons(x, fn() => interleave(ys, xf()) );

(*c*)
fun map_lazy_list f Nil = Nil
	| map_lazy_list f (Cons(x, xf))
	= Cons(f x, fn() => map_lazy_list f (xf()) );

(*d*)
fun iterates f x = Cons(x, fn() => iterates f (f x));

(*e*)
fun together x y = (x, y);
(*^^This is a function to put two values into a tuple so we can apply map with it*)
fun iterates2 f g x y = Cons(together x y, fn()=> 
	interleave(map_lazy_list (together x) (iterates g (g y)), iterates2 f g (f x) y ));
(*First part of interleave is x with the iterates of g on y and then the second part is f(x) with iterations of g on y and it recurses so we go up powers of f.*)