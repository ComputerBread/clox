#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include "value.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif



// ----------------------------------------------------------------------------
// parsing tokens
typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode; // are we in panic mode?
} Parser;

typedef enum {
    PREC_NONE,        // lowest precedence
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY      // highest precedence
} Precedence;


// (just a typedef, to use function pointer more easily.)
typedef void (*ParseFn)();

// one row of the parser table. (rules[])
typedef struct {
    ParseFn prefix; // to parse prefix expression
    ParseFn infix;  // to parse infix expression
    Precedence precedence;
} ParseRule;

Parser parser;

static void errorAt(Token* token, const char* message) {

    // if in panic mode, we suppress any other errors that get detected
    // Panic mode ens when the parser reaches a synchronization point (statement
    // boundaries)
    if (parser.panicMode) return;
    parser.panicMode = true;

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing.
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

static void advance() {
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.start);
    }
}

static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

// ----------------------------------------------------------------------------
// Emitting bytecode.
// after we parse & understand a piece of the user's program, the next step is
// to translate that to a series of bytecode instructions

Chunk* compilingChunk;

static Chunk* currentChunk() {
    return compilingChunk;
}

static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t b1, uint8_t b2) {
    emitByte(b1);
    emitByte(b2);
}

static void emitReturn() {
    emitByte(OP_RETURN);
}

/**
 * To insert an entry in the constant table
 */
static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        // this limit is pretty low
        // to increase it, we could add another instruction like OP_CONSTANT_16
        // that stores the index as a 2-byte operand
        error("Too many constants in one chunk.");
        return 0;
    }
    return (uint8_t)constant;
}

/**
 * First we add the value to the constant table (using makeConstant).
 * Then we emit an OP_CONSTANT instructiopn that pushes it onto the stack at
 * runtime.
 */
static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void endCompiler() {
    emitReturn();

#ifdef DEBUG_PRINT_CODE
    if(!parser.hadError) {
        disassembleChunk(currentChunk(), "code");
    }
#endif

}

// ----------------------------------------------------------------------------
// between parsing <- (this) -> emitting bytes

static void binary();
static void grouping();
static void parsePrecedence(Precedence precedence);
static ParseRule* getRule(TokenType type);
static void number();
static void unary();
static void expression();

// This syntax is called "designated initializers".
// Each TOKEN_... will be replaced by its numeric value and represents an index
// in the array.
// And {...} is just a struct init.
// This is used to parse 
ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] = {grouping, NULL, PREC_NONE},
    [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE}, 
    [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
    [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
    [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
    [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
    [TOKEN_BANG]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_BANG_EQUAL]    = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EQUAL_EQUAL]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_GREATER]       = {NULL,     NULL,   PREC_NONE},
    [TOKEN_GREATER_EQUAL] = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LESS]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LESS_EQUAL]    = {NULL,     NULL,   PREC_NONE},
    [TOKEN_IDENTIFIER]    = {NULL,     NULL,   PREC_NONE},
    [TOKEN_STRING]        = {NULL,     NULL,   PREC_NONE},
    [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
    [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FALSE]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_NIL]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_TRUE]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

/**
 * this is going to be called after the left part of the expression & the binary
 * operator have already been consumed.
 * This will function compiles the right operand.
 */
static void binary() {
    TokenType operatorType = parser.previous.type;

    // compile the right operand
    // Need to worry about precedence (so 2*3+4 only capture the "3", and not "3+4").
    // Binary operators are left-associative, so we will use ONE HIGHER LEVEL of
    // precedence. (we capture in the right operands any expression with higher
    // precedence than the one of the current operator).
    // Ex ( 1 + 2*3 + 4 => ((1+(2*3))+4) )
    // If we used the same level of precedence, we would get right-associativity
    // Ex: a = b = c => (a = (b = c))
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));

    // Emit the operator instruction.
    switch (operatorType) {
        case TOKEN_PLUS:          emitByte(OP_ADD); break;
        case TOKEN_MINUS:         emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR:          emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH:         emitByte(OP_DIVIDE); break;
        default: return; // Unreachable.
    }
}

/**
 * Starts at the current token & parses any expression at the given precedence
 * level or higher.
 *
 * The first token is always going to belong to some kind of prefix expression.
 * It can be followed by an infix expression.
 */
static void parsePrecedence(Precedence precedence) {

    // prefix expression
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expect expression.");
        return;
    }
    prefixRule();

    // infix expression
    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule();
    }
}

/**
 * this function exists solely to handle a declaration cycle in the C code.
 * yeah, well I didn't follow the boook properly and just put some prototypes
 * above!
 */
static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

/**
 * We assume the token for the nb literal has already been consumed & is stored
 * in parser.previous. We take that lexeme and use strtod to convert it to a
 * double value. Then we generate the code to load that value.
 */
static void number() {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void unary() {
    TokenType operatorType = parser.previous.type;

    // compile the operand
    parsePrecedence(PREC_UNARY);

    // emit the operator instruction.
    switch (operatorType) {
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        default:
            return;
    }
}


/**
* We assume the initial '(' has already been consumed.
* We recursively call back into expression() to compile the expression between
* the parentheses, then parse the closing ')' at the end.
*/
static void grouping() {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}



// ----------------------------------------------------------------------------
bool compile(const char* source, Chunk* chunk) {
    initScanner(source);

    compilingChunk = chunk;

    // init parser
    parser.hadError = false;
    parser.panicMode = false;

    advance();
    expression();
    consume(TOKEN_EOF, "Expect end of expression.");
    endCompiler();
    return !parser.hadError;
}
