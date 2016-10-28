//return sublist of H element starting from the B'th
list sublist(const list L, const int H, const int B) {
	return take(drop(L, B),H);	
}

//drop the first B element from the list
list drop(const list L, const int B) {
	if (L == nil) return L;
	if (B == 0) return L;
	return drop(tl(L), B-1);
}

//take the first H element of the list
list take(const list L, const int H) {
	if (L == nil) return L;
	if (H == 0) return nil;
	return cons(hd(L), take(tl(L), H-1));
}
