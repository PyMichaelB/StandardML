fun last ([],current) = current
	| 	last (x::rest, current) = last(rest, x);

(* It seems like there should be a more efficient way to do this because
this function runs through the entire list to get to the end. Looking ahead
in the notes I could use the functions take and drop to divide up the list. 
However, we need the length of the list which would involve going through 
each element so thus would not be more efficient*)
	