#ifndef clox_value_h
#define clox_value_h

#include "common.h"

// ----------------------------------------------------------------
// how do we represent the type of a value?
// how do we store the value itself?
// classic solution: tagged union (enum tag type + union).
// A value contains 2 parts:
// - a type "tag"
// - a payload (the actual value)
// A tagged union, combines these 2 parts into a single struct
// (in zig, the notation is easier!)

// the type tag
typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
} ValueType;

typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
    } as; // "as" is the name of the union, it will look nice
} Value;

// useful macro to deal with values:
// - to promote a native C value to a clox value:
#define BOOL_VAL(value)   ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL           ((Value){VAL_NIL,  {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
// - to access the underlying data
#define AS_BOOL(value)   ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
// - to test types
#define IS_BOOL(value)   ((value).type == VAL_BOOL)
#define IS_NIL(value)    ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)

// ----------------------------------------------------------------
// we can store code in chunks, but what about data?
// to compile statements containing literals, we need some sort of instruction
// to "produce a constant" and store those literals values.
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
