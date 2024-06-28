#ifndef clox_chunk_h
#define clox_chunk_h

// chunk refers to a sequences of bytecode

#include "common.h"
#include "value.h"

// in our bytecode format, each instruction has a one-byte operation code (opcode)
// that number controls what kind of instruction we're dealing with:
typedef enum
{
    OP_CONSTANT, // loads a constant for use
    OP_NIL,
    OP_TRUE,
    OP_FALSE,

    OP_POP, // to pop the stack, used by expressionStatement
    OP_DEFINE_GLOBAL,

    // binary operator
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
        // not defining OpCode for !=, <=, >= for education purpose
        // but we should do it for better perf!
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    // unary operator
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_RETURN,
} OpCode;

// dynamic array
typedef struct
{
    int capacity; // nb of elements in the array we have allocated
    int count;    // how many of those allocated entries are aactually in use
    uint8_t *code; // array of bytecodes
    int* lines; // array to store line number of corresponding bytecode
    ValueArray constants;
} Chunk;

void initChunk(Chunk *chunk);
void freeChunk(Chunk *chunk);
/*
    Appends a byte to the end of the chunk.
    If chunk doesn't have enough capacity, we double its capacity.
*/
void writeChunk(Chunk *chunk, uint8_t byte, int line);
int addConstant(Chunk *chunk, Value value);

#endif
