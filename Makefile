.PHONY: all clean

RADARE2_FLAGS=-Iradare2/libr/include $$(find ./radare2 -name 'libr_*.so')

all: bin/ bin/section_getter_client bin/ropc

bin/:
	mkdir bin

bin/libsection_getter.so: section_getter.c
	gcc ${RADARE2_FLAGS} -shared -fPIC $< -o $@

bin/section_getter_client: section_getter_client.c bin/libsection_getter.so
	gcc ${RADARE2_FLAGS} $^ -o $@

bin/ropc: ropc.hs bin/libsection_getter.so
	cabal configure
	cabal build
	cp dist/build/haskell-ropc/haskell-ropc bin/ropc

clean:
	rm -rf bin/ dist/
