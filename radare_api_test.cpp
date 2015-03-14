// export LD_LIBRARY_PATH=$(find ./radare2 -name '*.so' | sed 's|/[^/]*$||' | sort | uniq | sed "s|^\.|$(pwd)|" | tr '\n' ':')
// g++ -Iradare2/libr/include $(find ./radare2 -name 'libr_bin.so' -or -name 'libr_util.so' -or -name 'libr_io.so' -or -name 'libsdb.a') radare_api_test.cpp

#include <cstdio>
// http://blog.gmane.org/gmane.comp.tools.radare/month=20130601
extern "C" {
#include <r_bin.h>
#include <r_core.h>
#include <r_io.h>
#include <r_list.h>
}

/*
Some documentation/examples of (radare/libr)'s C API is at:
http://phrack.org/issues/66/14.html
but it seems to be somewhat outdated.
*/

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

int main(int argc, char* argv[]) {
    RIO* io = r_io_new();
    RBin* bin = r_bin_new();
    r_bin_iobind(bin, io);
    int rv;
    if(argc != 2) { printf("Usage: %s foo.elf\n", argv[0]); return 1; }
    char* fname = argv[1];
    rv = r_bin_load(bin, fname, 0, 0, 0, -1, 0);
    printf("r_bin_load: %d\n", rv);
    if(rv == 0) { return 1; }
    RBinObject* binobj = r_bin_get_object(bin);
    printf("binobj: 0x%016x\n", (size_t)binobj);
    if(binobj == 0) { return 1; }
    RList* sections = r_bin_get_sections(bin);
    if(sections == 0) { return 1; }
    printf("binobj->size: 0x%x\n", binobj->size);
    printf("sections: 0x%016x\n", (size_t)sections);
    RListIter* iter;
    void* voidSection;
    r_list_foreach(sections, iter, voidSection) {
        RBinSection* section = (RBinSection*)voidSection;
        if(!strcmp(".text", section->name)) {
            printf("0x%016x; 0x%016x\n", iter, section);
            printf("%s\n", section->name);
            printf("%x\n", section->size);
            printf("0x%x\n", binobj->baddr);
            printf("0x%x\n", binobj->loadaddr);
            printf("0x%x\n", r_bin_get_offset(bin));
            ut64 baddr = r_bin_get_baddr(bin);
            printf("0x%x\n", baddr);
            ut64 vaddr = r_bin_get_vaddr(bin, baddr, section->paddr, section->vaddr);
            printf("0x%x\n", vaddr);
            hexdump(1, (const void*)vaddr, section->size);
        }
    }
    r_bin_free(bin);
    r_io_free(io);
    return 0;
}
