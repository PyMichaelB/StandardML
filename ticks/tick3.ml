datatype 'a tree = Lf
				| Br of 'a * 'a tree * 'a tree;

fun tcons (v, Lf) = Br (v, Lf, Lf)
      | tcons (v, (Br (w, t1, t2))) = Br (v, tcons (w, t2), t1);
	
fun arrayoflist [] = Lf
	|	arrayoflist(x::xs) = tcons(x, arrayoflist(xs));
	
fun listofarray Lf = []
	|	listofarray (Br(v, t1, t2)) = 
		let fun convert ([], []) = []
		|	convert (xs, []) = xs
		|	convert ([], ys) = ys 
		|	convert(x::xs, y::ys) = x::y::convert(xs, ys)
		in v::convert(listofarray(t1), listofarray(t2))
	end;
		
fun getSubsOfEvens t = 
		let val listfromarray: int list = listofarray(t)
		fun testforevens ([], _) = []
			| testforevens (x::xs, n) = 
			if (x mod 2 = 0) then n::testforevens(xs, n+1)
			else testforevens(xs, n+1)
		in testforevens(listfromarray, 1)
	end;
	