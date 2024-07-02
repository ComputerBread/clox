#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "object.h"
#include "value.h"
#include "table.h"
#include <stdint.h>

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

// A  callframe represents a single ongoing function call
typedef struct {
    ObjFunction* function;
    uint8_t* ip; // return address
    Value* slots; // base pointer (I guess)
} CallFrame;

typedef struct {
    // function calls have their own stack
    // each function call creates a new callframe.
    // frames & framecount replace "chunk" and "ip"
    CallFrame frames[FRAMES_MAX];
    // number of ongoing function calls
    int frameCount;

    // stack-based VM
    // the stack:
    Value stack[STACK_MAX];
    // pointer to the element just past the element containing the top value
    // on the stack. So if stackTop points to stack[0], then stack is empty.
    Value* stackTop;

    // global variables
    Table globals;

    // Table (hashmap) of "interned strings", to avoid strings duplication,
    // and to make Tables work! (check string interning 20.5)
    // This allows to do == on strings! because same string address => same
    // string guarantee!
    Table strings;

    // pointer to the head of the list of all the object dynamically allocated
    // on the heap, so the garbage collector can free them!
    // (each Obj has a pointer to the next element in the list)
    Obj* objects;

} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value); // to the stack
Value pop();

#endif
