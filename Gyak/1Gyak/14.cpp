//increase each element of the list by one
list lista_noveltje2(const list L) {
	return map(novel_int, L);
}

//function to increase each element
int novel_int(int N) {
	return N+1;
}

//map implementation
list map(const fun1 F, const list L) {
	if (L == nil) return nil;
	return cons(F(hd(L)), map(F, tl(L)));
}
