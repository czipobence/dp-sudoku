//highest common divisor of A and B
int lnko(int A, int B) {
	if (B == 0) return A;
	return lnko(B,A % B);
}

