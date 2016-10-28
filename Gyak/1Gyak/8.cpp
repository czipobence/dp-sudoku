//insert an item to the nth position in the list
list insert_nth(const list L, int N, int E) {
	if (L == nil) return cons(E, nil);
	if (N == 0) return cons(E, L);
	return cons (hd(L), insert_nth(tl(L), N-1, E));
}
