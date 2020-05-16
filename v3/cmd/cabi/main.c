#include <stddef.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

void print_entry(const char* name, int size, int align, int falign) {
    printf("\t%s: {%d, %d, %d},\n", name, size, align, falign);
}

#define PRINT_TYPE(name, type) print_entry(name, sizeof(type), alignof(type), offsetof(struct{ char a; type b; }, b))

typedef enum {
    NONE
} Enum;

int main() {
    print_entry("Void", sizeof(void), alignof(void), alignof(void));
    PRINT_TYPE("Bool", bool);

    PRINT_TYPE("Char", char);
    PRINT_TYPE("SChar", signed char);
    PRINT_TYPE("UChar", unsigned char);

    PRINT_TYPE("Short", short);
    PRINT_TYPE("UShort", unsigned short);

    PRINT_TYPE("Enum", Enum);

    PRINT_TYPE("Int", int);
    PRINT_TYPE("UInt", unsigned int);

    PRINT_TYPE("Long", long);
    PRINT_TYPE("ULong", unsigned long);

    PRINT_TYPE("LongLong", long long);
    PRINT_TYPE("ULongLong", unsigned long long);

    PRINT_TYPE("Ptr", void*);
    print_entry("Function", sizeof(void(*)(void)), alignof(void(*)(void)), offsetof(struct{ char a; void(*b)(void); }, b));

    PRINT_TYPE("Float", float);
    PRINT_TYPE("Double", double);
    PRINT_TYPE("LongDouble", long double);


#ifdef __SIZEOF_INT128__
    PRINT_TYPE("Int128", __int128);
    PRINT_TYPE("UInt128", unsigned __int128);
#endif

#ifdef __GNUC__
    PRINT_TYPE("Float32", _Float32);
    PRINT_TYPE("Float32x", _Float32x);
    PRINT_TYPE("Float64", _Float64);
    PRINT_TYPE("Float64x", _Float64x);
    PRINT_TYPE("Float128", _Float128);

    PRINT_TYPE("Decimal32", _Decimal32);
    PRINT_TYPE("Decimal64", _Decimal64);
    PRINT_TYPE("Decimal128", _Decimal128);
#endif
}