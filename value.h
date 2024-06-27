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
//
// using a tagged union is good for fixed sized data types (bool, number, nil).
// But for dynamically sized values like strings, functions, instances... we
// need to allocate memory on the heap, so we will store a pointer in an "Obj".
// Each "Obj" will have a tag field to identifies its type. Each type will
// have its own separate struct, that will be used as a payload.
//
// (this is described in chapter 19, author call the technique "struct inheritance"
// but it's really just composition, but because we use Obj as the 1st field
// of other ObjWhatever (ex: ObjString) we can safely cast a ObjWhatever* to an
// Obj*). So any code that wants to work with all objects can treat them as base
// Obj* and ignore any other fields.
// You can also go the opposite direction (Obj* => ObjString*)

// The actual definition is inside "object.h"
typedef struct Obj Obj;
// then the payloads
typedef struct ObjString ObjString;

// the type tag
typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
    VAL_OBJ,
} ValueType;

typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
        Obj* obj;
    } as; // "as" is the name of the union, it will look nice
} Value;

// useful macro to deal with values:
// - to promote a native C value to a clox value:
#define BOOL_VAL(value)   ((Value){VAL_BOOL,   {.boolean = value}})
#define NIL_VAL           ((Value){VAL_NIL,    {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object)   ((Value){VAL_OBJ,    {.obj = (Obj*)object}})
// - to access the underlying data
#define AS_BOOL(value)   ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
#define AS_OBJ(value)    ((value).as.obj)
// - to test types
#define IS_BOOL(value)   ((value).type == VAL_BOOL)
#define IS_NIL(value)    ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_OBJ(value)    ((value).type == VAL_OBJ)

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

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray *array);
void writeValueArray(ValueArray *array, Value value);
void freeValueArray(ValueArray *array);
void printValue(Value value);

#endif
