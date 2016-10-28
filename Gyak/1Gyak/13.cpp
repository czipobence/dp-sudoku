//azon listaelemek, amiket azonos elem követ)
list parban(const list L) {
	if (L == nil) return L;
	return parban(tl(L), hd(L));
}

//azon listaelemek amiket azonos elem követ, 
//illetve a lista feje ha egyezik C-vel
list parban(const list L, int C) {
	if (L == nil) return nil;
	if (hd(L) == C) return cons(C, parban(tl(L), hd(L)));
	return parban(tl(L), hd(L));
}
