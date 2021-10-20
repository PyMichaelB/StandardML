fun nfold f 0 = (fn x => x)
	| nfold f n = (fn x => f(nfold f (n-1) x));

fun sum A B = nfold (fn x => x+1) A B;
fun product A B = nfold(fn x => B+x) A 0;
fun power A B = nfold(fn x => product A x) B 1; 
(*Computes A to the power of B*)

datatype 'a stream = Cons of 'a * (unit -> 'a stream);
fun from k = Cons(k, fn()=> from(k+1));

fun head (Cons(x, _)) = x;
fun tail (Cons(_, xf)) = xf();

fun nth(s, 1) = head(s)
	| nth(s, n) = nth(tail(s) , n-1);
	
fun square k = Cons(k*k, fn() => square(k+1));
val squares = square 1;

(*xs, ys are streams*)
fun map2 f xs ys  = Cons(f (head(xs)) (head(ys)), fn() => map2 f (tail(xs)) (tail (ys)) );