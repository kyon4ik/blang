libb: libb/all.b
	target/debug/blang -c libb/all.b -O
	ar rcs libb/libb.a libb/all.o
	
# libb-x86_64-linux: libb/all.b libb/x86_64-linux.s
# 	target/debug/blang -c libb/all.b -O -t x86_64-linux
# 	gcc -c libb/x86_64-linux.s -o libb/x86_64-linux.o
# 	ar rcs libb/libb.a libb/all.o libb/x86_64-linux.o

# libb-darwin: libb/all.b
# 	target/debug/blang -c libb/all.b -O
# 	ar rcs libb/libb.a libb/all.o
	
# libb-x86_64-darwin: libb/all.b libb/x86_64-darwin.s
# 	target/debug/blang -c libb/all.b -O -t x86_64-darwin
# 	clang -c libb/x86_64-darwin.s -o libb/x86_64-darwin.o
# 	ar rcs libb/libb.a libb/all.o libb/x86_64-darwin.o
