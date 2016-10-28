#include "cekla.h"

/* osszekevert(S, A) == SK, ha SK az S szám A alapú összekevert
   változata (S>0, A>1 egész számok).
   Egy S szám A alapú összekevertjét úgy kapjuk meg, hogy az S számot felírjuk
   az A alapú számrendszerben, a számjegyeit egy listával ábrázoljuk, a
   listát a fentiek szerint átrendezzük, majd a kapott lista elemeit
   egy A alapú szám jegyeinek tekintve elõállítjuk a keresett értéket.
*/
int osszekevert(const int S, const int A) {
	list lst = toList(S,A);
	if (tl(lst) == nil) {
		return S;
	}
	return toInt(merge(getOdd(lst), revert(getOdd(tl(lst)))), A);
}

/* 
 * átalakítja S számot számjegyek sorozatára A alapú számrendszerben
 * a lista elejére a legnagyobb, végére a legkisebb helyiértékû számjegy
 * kerül
 */
list toList(const int S, const int A) {
	return toList(S,A,nil);
}

/*
 * átalakítja S számot számjegyek sorozatára A alapú számrendszerben,
 * majd utánafûzi az L listát. A lista elejére a legnagyobb, végére a 
 * legkisebb helyiértékû számjegy kerül
 */
list toList(const int S, const int A, const list L) {
	if (S == 0) return L;
	return toList(S/A, A, cons(S%A, L));
}

//L lista megfordítása
list revert (const list L) {
	return revert(L, nil);
}

//L listát megfordítva M lista végére fûzi 
list  revert(const list L, const list M) {
	if (L == nil) return M;
	return revert(tl(L), cons(hd(L), M));
}

/*
 * L lista, mint számjegyek helyiérték szerint csökkenõ sorozata A alapú 
 * számrendszeben egész számmá visszaalakítva
 */
int toInt(const list L, const int A) {
	return toInt(L,A,0);
}

/*
 * L lista, mint számjegyek helyiérték szerint csökkenõ sorozata A alapú 
 * számrendszeben egész számmá visszaalakítva és Acc értékének A szorosa
 */
int toInt(const list L, const int A, int Acc) {
	if (L == nil) return Acc;
	return toInt(tl(L), A, hd(L) + Acc * A);
}

/*
 * Visszaad egy listát, mely az eredeti lista páratlanadik sorszámú
 * elemeit tartalmazza
 */
list getOdd(const list L) {
	return revert(getOddReverted(L, nil));
}

/*
 * Visszaad egy listát, mely L lista páratlanadik sorszámú elemeit
 * tartalmazza M lista elejére fûzve fordított sorrendben
 */ 
list getOddReverted(const list L, const list M)  {
	if (L == nil) return M;
	if (tl(L) == nil) return cons(hd(L), M);
	return getOddReverted(tl(tl(L)),cons(hd(L), M));
}

/*
 * L1 és L2 lista váltogtva összefûzése, a lista elejére L1 elsõ eleme
 * kerül, majd L2 elsõ eleme, majd L1 második eleme, stb..
 */ 
list merge(const list L1, const list L2) {
	if (L1 == nil) return L2;
	if (L2 == nil) return L1;
	return cons(hd(L1), cons(hd(L2), merge(tl(L1), tl(L2))));
}
