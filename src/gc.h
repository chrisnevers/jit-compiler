#ifndef GC_HEADER
#define GC_HEADER

#include <iostream>

const size_t heap_size = 1 << 10;
const size_t half_size = heap_size / 2;

void* malloc1 (size_t size);
void print_space (char* begin, char* end, const char* label);

#endif
