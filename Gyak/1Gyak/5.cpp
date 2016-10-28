//get the length of the list
int length(const list L) {
	return length(L, 0);
}

//get the length of the list + i
int length(const list L, int i) {
	if (L == nil) {
		return i;
	}
	return length(tl(L), i+1);
}
