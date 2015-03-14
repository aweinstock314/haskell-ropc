// gcc -Iradare2/libr/include $(find ./radare2 -name 'libr_bin.so' -or -name 'libr_util.so' -or -name 'libr_io.so' -or -name 'libsdb.a') -DDEBUG -shared -fPIC section_getter.c -o section_getter.so
// gcc -Iradare2/libr/include $(find ./radare2 -name 'libr_bin.so' -or -name 'libr_util.so' -or -name 'libr_io.so' -or -name 'libsdb.a') -shared -fPIC section_getter.c -o section_getter.so
// gcc -Iradare2/libr/include $(find ./radare2 -name 'libr_bin.so' -or -name 'libr_util.so' -or -name 'libr_io.so' -or -name 'libsdb.a') -fPIC -DEMIT_MAIN section_getter.c 
#include <r_bin.h>
#include <r_core.h>
#include <r_io.h>
#include <r_list.h>

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef DEBUG
static char hexdigit(char nybble) {
    //printf("Nybble: %x\n", nybble);
    if((0x0 <= nybble) && (nybble <= 0x9)) { return '0'+nybble; }
    if((0xA <= nybble) && (nybble <= 0xF)) { return 'A'+nybble-0xA; }
    return '?';
}

static void hexdump(int fd, const void* buffer, size_t count) {
    const char* p = (const char*)buffer;
    char outbuf[2];
    while(count--) {
        outbuf[0] = hexdigit(((*p) & 0xf0) >> 4);
        outbuf[1] = hexdigit((*p) & 0x0f);
        write(fd, outbuf, 2);
        p++;
    }
}
#define dbgprintf printf
#else
#define hexdump(...)
#define dbgprintf(...)
#endif

char* get_section_by_name(const char* filename, const char* sectionname, size_t* out_size) {
    dbgprintf("filename: %s, sectionname: %s\n", filename, sectionname);
    char* result = NULL;
    *out_size = 0;
    int fd = open(filename, O_RDONLY); if(fd == -1) { goto cleanup0; }
    RIO* io = r_io_new(); if(io == NULL) { goto cleanup1; }
    RBin* bin = r_bin_new(); if(bin == NULL) { goto cleanup2; }
    r_bin_iobind(bin, io); // this has void type rather than a return code, so assume it can't fail?
    if(r_bin_load(bin, filename, 0, 0, 0, -1, 0) == 0) { goto cleanup3; }
    RList* sections = r_bin_get_sections(bin);
    if(sections == NULL) { goto cleanup3; }
    RListIter* iter; RBinSection* section;
    dbgprintf("reached the loop\n");
    r_list_foreach(sections, iter, section) {
        dbgprintf("considering section %s\n", section->name);
        if(!strcmp(sectionname, section->name)) {
            result = malloc(section->size);
            if(result == NULL) { goto cleanup3; }
            dbgprintf("result ptr: 0x%016x\n", result);
            *out_size = section->size;
            size_t offset = section->paddr;
            // TODO: error handling (free result if lseek or read fail)
            lseek(fd, section->paddr, SEEK_SET);
            read(fd, result, section->size);
            goto cleanup3;
        }
    }
    cleanup3:
    r_bin_free(bin);
    cleanup2:
    r_io_free(io);
    cleanup1:
    close(fd);
    cleanup0:
    return result;
}

#ifdef EMIT_MAIN
int main() {
    size_t size = 0;
    void* result = get_section_by_name("./a.out", ".text", &size);
    printf("Result from get_section_by_name (%u bytes):\n", size);
    hexdump(1, result, size);
    printf("\n");
}
#endif
