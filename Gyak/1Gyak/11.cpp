//drop the first B element from the list
list drop(const list L, const int B) {
	if (L == nil) return L;
	if (B == 0) return L;
	return drop(tl(L), B-1);
}
