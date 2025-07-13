libb-aarch64-darwin: libb/all.b libb/aarch64.s
	target/debug/blang -c libb/all.b -O -t aarch64-darwin
	clang -c libb/aarch64.s -o libb/aarch64.o
	ar rcs libb/libb.a libb/all.o libb/aarch64.o
	
libb-x86_64-linux: libb/all.b libb/x86_64.s
	target/debug/blang -c libb/all.b -O -t x86_64-linux
	gcc -c libb/x86_64.s -o libb/x86_64.o
	ar rcs libb/libb.a libb/all.o libb/x86_64.o
	
libb-x86_64-darwin: libb/all.b libb/x86_64.s
	target/debug/blang -c libb/all.b -O -t x86_64-darwin
	clang -c libb/x86_64.s -o libb/x86_64.o
	ar rcs libb/libb.a libb/all.o libb/x86_64.o
