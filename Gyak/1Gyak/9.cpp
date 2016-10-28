//returns the n'th element of the list
int nth(list L, int N) {
	if (L == nil) return 0;
	if (N == 1) return hd(L);
	return nth(tl(L), N-1);
}
