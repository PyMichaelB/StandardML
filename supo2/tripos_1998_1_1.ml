fun minimum_out [] = ([],[])
	| minimum_out [x] = ([x], [])
	| minimum_out (x::xs : real list) = 
		let val([a], without_a) = minimum_out(xs)
		in if x<a then ([x], xs)
			else ([a], x::without_a)
		end;
		
fun least(0, xs: real list) = []
	| least(n, xs) = 
	let val ([min], wo_min) = minimum_out(xs)
	in min::least(n-1, wo_min)
	end;
