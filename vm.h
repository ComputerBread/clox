#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "value.h"
#include "table.h"
#include <stdint.h>

#define STACK_MAX 256

typedef struct {
    Chunk* chunk; // chunk the VM is executing

    // instruction pointer (pointer to the next instruction to execute)
    // points directly to the instruction (faster than using the index)
    uint8_t* ip; 

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
