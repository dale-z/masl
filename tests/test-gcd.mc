int gcd(int a, int b) {
  while (a != b) {
    if (a > b) {
		a = a - b;
	} else {
		b = b - a;
	}
	return a;
}

printInt(gcd(2,14));
printChar('\n');
printInt(gcd(3,15));
printChar('\n');
printInt(gcd(99,121));
printChar('\n');
