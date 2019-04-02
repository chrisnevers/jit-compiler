#include <iostream>
#include <stdexcept>
#include "gc.h"

extern size_t heap[];
extern size_t heap_max;

using namespace std;

void* malloc1 (int size) {
    if (size > heap_max) {
        throw logic_error ("GC: Out of memory!");
    }
    return malloc (size);
}
