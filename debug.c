#include <stdio.h>
#include "debug.h"

static int simpleInstruction(const char *name, int offset);

void disassembleChunk(Chunk *chunk, const char *name)
{
    printf("== %s ==\n", name);

    for (int offset = 0; offset < chunk->count;)
    {
        offset = disassembleInstruction(chunk, offset);
    }
}

/**
 * disassemble an instruction at the given offset and returns the offset of the
 * next instruction.
 */
int disassembleInstruction(Chunk *chunk, int offset)
{
    // print byte offset of current instruction
    printf("%04d ", offset);

    uint8_t instruction = chunk->code[offset];
    switch (instruction)
    {
    case OP_RETURN:
        return simpleInstruction("OP_RETURN", offset);
    default:
        printf("Unknown opcode: %d", instruction);
        return offset + 1;
    }
}

static int simpleInstruction(const char *name, int offset)
{
    printf("%s\n", name);
    return offset + 1;
}