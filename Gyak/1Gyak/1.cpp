//1, if N consist only of 0 and 1 digits, otherwise 0
int csupa01(const int N) {
	if (N == 0) return 1;
	if (N % 10 > 1) return 0;
	return csupa01(N/10);
}
