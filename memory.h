#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"

#define GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity)*2)

#define GROW_ARRAY(type, pointer, oldCount, newCount) \
    (type*)reallocate(pointer, sizeof(type) * (oldCount), sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount) \
    reallocate(pointer, sizeof(type) * (oldCount), 0)

/**
The single function used for all dynamic memory management: allocating memory,
freeing it, changing the size of an existing allocation.
The 2 size arguments control which operation to perform:

old: 0,        new: non-zero  => allocate memory
old: non-zero, new: 0         => free allocation
old: non-zero, new: < oldSize => shink
old: non-zero, new: > oldSize => grow
*/
void* reallocate(void* pointer, size_t oldSize, size_t newSize);

#endif