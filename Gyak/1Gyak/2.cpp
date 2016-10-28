#include "cekla.h"

//divisors of n
int osztok(int n) {
	return osztok(n,n,0);
}

//n's divisors that are smaller or equals than k + the value of c
int osztok(int n, int k, int c) {
	if (k== 0) return c;
	if ((n%k) == 0) return osztok (n,k-1,c+1);
	return osztok(n,k-1,c);
}
