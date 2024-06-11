#include <stdlib.h>
#include "chunk.h"
#include "memory.h"

void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
}

void freeChunk(Chunk* chunk) {
    // deallocate all the memory
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    // zero out the fields leaving the chunk in a well-defined empty state
    initChunk(chunk);
}

void writeChunk(Chunk* chunk, uint8_t byte) {

    // if chunk is full, we increase its capacity
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
    }

    chunk->code[chunk->count] = byte;
    chunk->count++;
}