#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "value.h"

#define OBJ_TYPE(value)        (AS_OBJ(value)->type)

#define IS_STRING(value)       isObjType(value, OBJ_STRING)
#define IS_FUNCTION(value)     isObjType(value, OBJ_FUNCTION)

#define AS_STRING(value)       ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value)      (((ObjString*)AS_OBJ(value))->chars)
#define AS_FUNCTION(value)     (((ObjFunction*)AS_OBJ(value)))

typedef enum {
  OBJ_FUNCTION,
  OBJ_STRING,
} ObjType;

struct Obj {
  ObjType type;

  // pointer to next Obj in the chain
  // used to keep track of all object allocated on the head
  // useful for garbage collector!
  struct Obj* next;
};

// Each function has its own Chunk of code & other metadata
// Functions are first class in Lox
typedef struct {
  Obj obj;
  int arity; // nb of parameters the func expects
  Chunk chunk;
  ObjString* name; // function's name, to have better errors
} ObjFunction;

// Given an ObjString* you can safely cast if to Obj* (because Obj is the first
// field). Every ObjString "is" an Obj in the OOP sense.
struct ObjString {
    Obj obj;
    int length;
    char* chars;

    // this is used for the hashtable, if a string is used as a key, we only
    // hash it once & store it here
    uint32_t hash;
};

// to create a new Lox function
ObjFunction* newFunction();

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

ObjString* copyString(const char* chars, int length);

void printObject(Value value);
ObjString* takeString(char* chars, int length);



#endif
