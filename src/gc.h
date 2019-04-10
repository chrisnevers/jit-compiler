#ifndef GC_HEADER
#define GC_HEADER

#include <iostream>

const size_t heap_size = 1 << 10;
const size_t half_size = heap_size / 2 * sizeof(size_t);

void* malloc1 (size_t size);
void print_space (size_t* begin, size_t* end, const char* label);

#endif
