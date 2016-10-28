#include "cekla.h"

/* osszekevert(S, A) == SK, ha SK az S sz�m A alap� �sszekevert
   v�ltozata (S>0, A>1 eg�sz sz�mok).
   Egy S sz�m A alap� �sszekevertj�t �gy kapjuk meg, hogy az S sz�mot fel�rjuk
   az A alap� sz�mrendszerben, a sz�mjegyeit egy list�val �br�zoljuk, a
   list�t a fentiek szerint �trendezz�k, majd a kapott lista elemeit
   egy A alap� sz�m jegyeinek tekintve el��ll�tjuk a keresett �rt�ket.
*/
int osszekevert(const int S, const int A) {
	list lst = toList(S,A);
	if (tl(lst) == nil) {
		return S;
	}
	return toInt(merge(getOdd(lst), revert(getOdd(tl(lst)))), A);
}

/* 
 * �talak�tja S sz�mot sz�mjegyek sorozat�ra A alap� sz�mrendszerben
 * a lista elej�re a legnagyobb, v�g�re a legkisebb helyi�rt�k� sz�mjegy
 * ker�l
 */
list toList(const int S, const int A) {
	return toList(S,A,nil);
}

/*
 * �talak�tja S sz�mot sz�mjegyek sorozat�ra A alap� sz�mrendszerben,
 * majd ut�naf�zi az L list�t. A lista elej�re a legnagyobb, v�g�re a 
 * legkisebb helyi�rt�k� sz�mjegy ker�l
 */
list toList(const int S, const int A, const list L) {
	if (S == 0) return L;
	return toList(S/A, A, cons(S%A, L));
}

//L lista megford�t�sa
list revert (const list L) {
	return revert(L, nil);
}

//L list�t megford�tva M lista v�g�re f�zi 
list  revert(const list L, const list M) {
	if (L == nil) return M;
	return revert(tl(L), cons(hd(L), M));
}

/*
 * L lista, mint sz�mjegyek helyi�rt�k szerint cs�kken� sorozata A alap� 
 * sz�mrendszeben eg�sz sz�mm� visszaalak�tva
 */
int toInt(const list L, const int A) {
	return toInt(L,A,0);
}

/*
 * L lista, mint sz�mjegyek helyi�rt�k szerint cs�kken� sorozata A alap� 
 * sz�mrendszeben eg�sz sz�mm� visszaalak�tva �s Acc �rt�k�nek A szorosa
 */
int toInt(const list L, const int A, int Acc) {
	if (L == nil) return Acc;
	return toInt(tl(L), A, hd(L) + Acc * A);
}

/*
 * Visszaad egy list�t, mely az eredeti lista p�ratlanadik sorsz�m�
 * elemeit tartalmazza
 */
list getOdd(const list L) {
	return revert(getOddReverted(L, nil));
}

/*
 * Visszaad egy list�t, mely L lista p�ratlanadik sorsz�m� elemeit
 * tartalmazza M lista elej�re f�zve ford�tott sorrendben
 */ 
list getOddReverted(const list L, const list M)  {
	if (L == nil) return M;
	if (tl(L) == nil) return cons(hd(L), M);
	return getOddReverted(tl(tl(L)),cons(hd(L), M));
}

/*
 * L1 �s L2 lista v�ltogtva �sszef�z�se, a lista elej�re L1 els� eleme
 * ker�l, majd L2 els� eleme, majd L1 m�sodik eleme, stb..
 */ 
list merge(const list L1, const list L2) {
	if (L1 == nil) return L2;
	if (L2 == nil) return L1;
	return cons(hd(L1), cons(hd(L2), merge(tl(L1), tl(L2))));
}
