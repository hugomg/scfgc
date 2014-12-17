all:
	./build.sh main.native main.byte

linux:
	./build.sh main.native
	cp _build/main.native _build/scfgc

windows:
	./build.sh main.native
	cp _build/main.native _build/scfgc.exe

.PHONY: clean

clean:
	./build.sh -clean
	rm -f ./*.native ./*.byte



