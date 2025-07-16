libb.a: libb/all.b target/debug/blang
	target/debug/blang -c libb/all.b -O
	ar rcs libb/libb.a libb/all.o
	
clean:
	rm libb/*.o
	rm libb/libb.a
