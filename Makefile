ifeq ($(OS),Windows_NT)
	COMPILER_PATH := target/debug/blang.exe
	AR            := lib /OUT:
	LIB_EXT       := lib    
else
	COMPILER_PATH := target/debug/blang
	AR            := ar rcs
	LIB_EXT       := a
endif

libb/libb.$(LIB_EXT): libb/all.b $(COMPILER_PATH)
	$(COMPILER_PATH) -c libb/all.b -O
	$(AR) libb/libb.$(LIB_EXT) libb/all.o

.PHONY: clean
clean:
	rm libb/*.o
	rm libb/libb.a
