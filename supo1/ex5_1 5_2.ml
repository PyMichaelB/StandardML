(*Exercise 5.1*)
(*Time complexity is O(n^2) because to search for the minimum you require n-1 comparisons.
Then, to search for the next minimum you need n-2 comparisons and for the next minimum n-3 
comparisons and so on.
(n-1) + (n-2) + (n-3) + ... + 1 = (1/2)(n)(n-1) = (1/2)(n^2-n). Then, the minimal element must be swapped
but this takes constant time.*)

/*Exercise 5.2*/
fun findMinimum [] = raise Empty
	| findMinimum [a] = (a, [])
	| findMinimum (a::b) = 
	