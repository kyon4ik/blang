func(i) {
	extrn printf, func;
	if (i > 0) {
		printf("%d*n", i);
		func(i-1);
	}
}

main() {
	extrn func;
	func(10);
}
