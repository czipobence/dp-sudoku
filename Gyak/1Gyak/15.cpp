//sum up list elements
list sum(const list L) {
	return foldl(add, 0, L);
}

//sum up two integers
int add(const int A, const int B) {
	return A + B;
}

//map implementation
list map(const fun1 F, const list L) {
	if (L == nil) return nil;
	return cons(F(hd(L)), map(F, tl(L)));
}

// foldl(F, a, [x1,...,xn]) = F(xn, ..., F(x2, F(x1, a))...)
int foldl(const fun2 F, const int Acc, const list L) {
	if (L == nil) return Acc;
	else return foldl(F, F(hd(L),Acc), tl(L));
}

// foldr(F, a, [x1, ..., xn]) = F(x1, F(x2, ..., F(xn, a)...))
int foldr(const fun2 F, const int Acc, const list L) {
	if (L == nil) return Acc;
	else return F(hd(L), foldr(F, Acc, tl(L))); 
}
