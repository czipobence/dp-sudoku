//visszaadja a ket szam legnagyobb kozos osztojat
int lnko(int A, int B) {
	return ko(A,B,A);
}

//visszaadja a ket szam legnagyobb, de help-nel kisebb kozos osztojat
int ko(int A, int B, int help) {
	if (A % help == 0) {
		if (B % help == 0) {
			return help;
		}
	}
	return ko(A,B,help-1);
}
