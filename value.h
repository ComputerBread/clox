#ifndef clox_value_h
#define clox_value_h

#include "common.h"

// for now, we'll only support double-precision floating point numbers
// we will expand this over time
typedef double Value;

// ----------------------------------------------------------------
// we can store code in chunks, but what about data?
// to compile statements containing literals, we need some sort of instruction
// to "produce a consant" and store those literals values.
// We will do like the JVM and use a constant pool.
// A constant pool is an array of values.
// it will contain all(?) the literals/constants in our program
typedef struct
{
    int capacity;
    int count;
    Value *values;
} ValueArray;

void initValueArray(ValueArray *array);
void writeValueArray(ValueArray *array, Value value);
void freeValueArray(ValueArray *array);
void printValue(Value value);

#endif
