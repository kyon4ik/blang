foo(a) {
	return (a);
}

main() {
	extrn printf, foo;
	auto a;
	a = foo;
	printf("%d*n", a(42));
	printf("%d*n", (&*foo)(69)); 
}
