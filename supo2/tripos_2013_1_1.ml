(* part (a) Datatypes can be declared in ML. For example, to declare the 
datatype for a tree we can use the following:

datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;

The datatype is composed of the type itself and constructors. The
constructors are the identifiers of the type. Pattern matching is when
arguments of a function which obey a certain pattern cause different
operations to take place. For example, the function below sums the
elements in a list:

fun sum ([], n) = n
	| sum(x::xs, n) = sum(xs, n+x);
	
If the argument of the function sum is the empty list with a number as a tuple then the sum must
be that number (initially it is zero). However, if it is non-empty then we need to compute the sum by
other means. This involves an iterative call of sum and eventually the first
pattern is called when we reach the end of our list.*)

(*b*)
(*Takes first tree from the left and adds it to the right trees*)
datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;

fun addright v t1 [] = []
	| addright v t1 (y::ys) = Br(v, t1, y)::addright v t1 ys;
	
(*Goes through each left tree and then uses addright to make list of all
trees*)
fun addleft v [] _ = []
	| addleft v (x::xs) ys = addright v x ys @ addleft v xs ys;

(*c*)
(*Takes nth element of a list*)
fun nth 0 (x::_) = x
  | nth n (x::xs) = nth (n-1) xs;

(*Removes and keeps first n elements*)
fun take (0,x::xs) = []
  | take (n,x::xs) = x::take(n-1,xs);

(*Removes and keeps last n elements*)
fun drop (0,x::xs) = xs
  | drop (n,x::xs) = drop(n-1,xs);

  
fun inorderTrees [] i = [Lf]
  | inorderTrees (x::xs) i = addleft (nth i (x::xs)) (inorderTrees (take (i, x::xs)) 0) 
  (inorderTrees (drop(i,x::xs)) 0) @ 
  inorderTrees (x::xs) (i+1) handle _ => 
  addleft (nth i (x::xs)) (inorderTrees (take (i, x::xs)) 0) 
  (inorderTrees (drop(i,x::xs)) 0);
(*Handle is when i gets greater than the length of list*)