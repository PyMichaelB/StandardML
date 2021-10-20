fun sumList [] = 0
    |	sumList (x::rest) = x + sumList (rest);
	
fun sumListI ([],tot) = tot
	| 	sumListI (x::rest, tot) = sumListI(rest, tot+x);
	
(* For large lists, the second (iterative) function is probably more efficient in terms of space.
For a list of n elements, both functions need to be called n times but the first function has to 
add up the results of its computations at the end (recursion) and so the iterative would be more
efficient but their complexity in O notation is probably the same so the difference is not 
significant.*)