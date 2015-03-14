// gcc ./section_getter.so section_getter_client.c -o section_getter_client
#include <stdio.h>

extern char* get_section_by_name(const char* filename, const char* sectionname, size_t* out_size);

char hexdigit(char nybble) {
    //printf("Nybble: %x\n", nybble);
    if((0x0 <= nybble) && (nybble <= 0x9)) { return '0'+nybble; }
    if((0xA <= nybble) && (nybble <= 0xF)) { return 'A'+nybble-0xA; }
    return '?';
}

void hexdump(int fd, const void* buffer, size_t count) {
    const char* p = (const char*)buffer;
    char outbuf[2];
    while(count--) {
        outbuf[0] = hexdigit(((*p) & 0xf0) >> 4);
        outbuf[1] = hexdigit((*p) & 0x0f);
        write(fd, outbuf, 2);
        p++;
    }
}

int main() {
    size_t size;
    void* result = get_section_by_name("./a.out", ".text", &size);
    printf("Result from get_section_by_name (%u bytes):\n", size);
    hexdump(1, result, size);
    printf("\n");
}
