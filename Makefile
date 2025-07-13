libb-aarch64-linux: libb/all.b libb/aarch64-linux.s
	target/debug/blang -c libb/all.b -O -t aarch64-linux
	clang -c libb/aarch64-linux.s -o libb/aarch64-linux.o
	ar rcs libb/libb.a libb/all.o libb/aarch64-linux.o
	
libb-x86_64-linux: libb/all.b libb/x86_64-linux.s
	target/debug/blang -c libb/all.b -O -t x86_64-linux
	gcc -c libb/x86_64-linux.s -o libb/x86_64-linux.o
	ar rcs libb/libb.a libb/all.o libb/x86_64-linux.o

libb-aarch64-darwin: libb/all.b libb/aarch64-darwin.s
	target/debug/blang -c libb/all.b -O -t aarch64-darwin
	clang -c libb/aarch64-darwin.s -o libb/aarch64-darwin.o
	ar rcs libb/libb.a libb/all.o libb/aarch64-darwin.o
	
libb-x86_64-darwin: libb/all.b libb/x86_64-darwin.s
	target/debug/blang -c libb/all.b -O -t x86_64-darwin
	clang -c libb/x86_64-darwin.s -o libb/x86_64-darwin.o
	ar rcs libb/libb.a libb/all.o libb/x86_64-darwin.o
