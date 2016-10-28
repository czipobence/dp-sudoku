//increase each element of the list by one
list novel(const list L) {
	if (L == nil) return L;
	return cons(hd(L) +1, novel(tl(L)));
}
