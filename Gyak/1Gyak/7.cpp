//get the last element of a list
int last(const list L) {
	if (L == nil) return 0;
	if (tl(L) == nil) return hd(L);
	return last(tl(L));
}
