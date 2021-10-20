fun nbreath [] = []
	| nbreath (Lf::ts) = nbreath ts
	| nbreath (Br(v, t, u)::ts) = v::nbreath(ts@[t, u]);
	