.PHONY: all clean

RADARE2_FLAGS=-Iradare2/libr/include $$(find ./radare2 -name 'libr_*.so')

all: bin/section_getter_client

bin/:
	mkdir bin

bin/section_getter.so: section_getter.c bin/
	gcc ${RADARE2_FLAGS} -shared -fPIC $< -o $@

bin/section_getter_client: section_getter_client.c bin/section_getter.so
	gcc ${RADARE2_FLAGS} $^ -o $@

clean:
	rm -rf bin/
