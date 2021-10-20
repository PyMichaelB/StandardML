fun rotations ([]: 'a list) = [[]]
	| rotations(x::xs) = [x] @ rotations(xs);
	