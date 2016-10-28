//take the first H element of the list
list take(const list L, const int H) {
	if (L == nil) return L;
	if (H == 0) return nil;
	return cons(hd(L), take(tl(L), H-1));
}
