#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

#define OBJ_TYPE(value)        (AS_OBJ(value)->type)

#define IS_STRING(value)       isObjType(value, OBJ_STRING)

#define AS_STRING(value)       ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value)      (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
  OBJ_STRING,
} ObjType;

struct Obj {
  ObjType type;

  // pointer to next Obj in the chain
  // used to keep track of all object allocated on the head
  // useful for garbage collector!
  struct Obj* next;
};

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


static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

ObjString* copyString(const char* chars, int length);

void printObject(Value value);
ObjString* takeString(char* chars, int length);



#endif
