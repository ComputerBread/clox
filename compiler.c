#include <stdint.h>
#include <string.h>
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
typedef void (*ParseFn)(bool canAssign);

// one row of the parser table. (rules[])
typedef struct {
    ParseFn prefix; // to parse prefix expression
    ParseFn infix;  // to parse infix expression
    Precedence precedence;
} ParseRule;


typedef struct {
    Token name;
    int depth;
} Local;

typedef struct {
    // simple flat array of all locals that are in scope during compilation
    // they are ordered in the order that their declarations appear in the code.
    // Since our bytecode operand is a single byte, we can only store UINT8_MAX+1
    // variables.
    Local locals[UINT8_COUNT];
    int localCount;
    int scopeDepth; // 0 = global scope
} Compiler;

Parser parser;

Compiler* current = NULL;

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

static bool check(TokenType type) {
    return parser.current.type == type;
}

static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
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

static void initCompiler(Compiler* compiler) {
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    current = compiler;
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

static void binary(bool canAssign);
static void grouping(bool canAssign);
static void parsePrecedence(Precedence precedence);
static ParseRule* getRule(TokenType type);
static void number(bool canAssign);
static void unary(bool canAssign);
static void expression();
static void statement();
static void declaration();
static void literal(bool canAssign);
static void string(bool canAssign);
static void variable(bool canAssign);

// This syntax is called "designated initializers".
// Each TOKEN_... will be replaced by its numeric value and represents an index
// in the array.
// And {...} is just a struct init.
// This is used to parse 
ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE},
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
    [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
    [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
    [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
    [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
    [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
    [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
    [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
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
static void binary(bool canAssign) {
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
        case TOKEN_BANG_EQUAL:    emitBytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL); break;
        case TOKEN_GREATER:       emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS:          emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL:    emitBytes(OP_GREATER, OP_NOT); break;
        case TOKEN_PLUS:          emitByte(OP_ADD); break;
        case TOKEN_MINUS:         emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR:          emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH:         emitByte(OP_DIVIDE); break;
        default: return; // Unreachable.
    }
}

static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_NIL:   emitByte(OP_NIL);   break;
        case TOKEN_TRUE:  emitByte(OP_TRUE);  break;
        default:
            return; // Unreachable
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

    // Assignment are a bit tricky:
    // to prevent things like: a * b = c + d; (syntax error), we need to know
    // if a variable happens to be the right-hand side of an infix operator
    // or the operand of a unary operator, then that containing expression is
    // too high precedence to permit the "="
    // (a*b has higher precedence than "b=c+d")
    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    // infix expression
    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    // if we are in situation where user tries to make an illegal assignment
    // nothing is going to consume the "=", so if that's the case, we make
    // an error.
    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target");
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

static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static void addLocal(Token name) {

    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in scope.");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = current->scopeDepth;
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static void declareVariable() {
    // global var are implicitly declared
    if (current->scopeDepth == 0) return;

    Token* name = &parser.previous;

    // it's an error to have 2 variables with the same name in the same local scope
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already variable with this name in this scope.");
        }
    }

    addLocal(*name);
}

static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    // if local variable, we exit
    // because they are not looked up by name
    if (current->scopeDepth > 0) return 0;

    // globals are looked up by name
    return identifierConstant(&parser.previous);
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

static void defineVariable(uint8_t global) {
    // for local variable we have nothing to do, the value is already on the
    // stack
    if (current->scopeDepth > 0) return;

    emitBytes(OP_DEFINE_GLOBAL, global);
}

static void varDeclaration() {
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        // if no initializer, we init variable to nil.
        emitByte(OP_NIL);
    }

    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(global);
}

/**
 * An expression statement evaluates the expression and discards the result.
 * Usually, it's so that you can call a function or evaluate an assignment for
 * its side effect.
 * ex1 : brunch = "cookie";
 * ex2 : print(variable);
 */
static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void synchronize() {
    parser.panicMode = false;

    // we skip tokens until we reach something that looks like a statement
    // boundary. (a semicolon, or the beginning of a statement)
    while (parser.current.type != TOKEN_EOF) {

        // end of statement
        if (parser.previous.type == TOKEN_SEMICOLON) return;

        switch (parser.current.type) {
            // beginning of a statement
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;

            // everything else is skipped
            default:
                ; // Do nothing.
        }

        advance();
    }

}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    while (current->localCount > 0
        && current->locals[current->localCount-1].depth > current->scopeDepth) {
        emitByte(OP_POP);
        current->localCount--;
    }
}

static void block() {
    while(!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_LEFT_BRACE)){
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

static void declaration() {
    if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    // synchronize panic mode
    if (parser.panicMode) synchronize();
}

/**
 * We assume the token for the nb literal has already been consumed & is stored
 * in parser.previous. We take that lexeme and use strtod to convert it to a
 * double value. Then we generate the code to load that value.
 */
static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void string(bool canAssign) {
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length-2)));
    // the +1 & -2 parts trim the leading & trailing quotation marks
    // (if Lox supported string escape sequences like `\n`, we'd translate those here)
}

static int resolveLocal(Compiler* compiler, Token* name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if(identifiersEqual(name, &local->name)) {
            return i;
        }
    }

    return -1;
}

static void namedVariable(Token name, bool canAssign) {

    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    // we check if next token is "=", to know if this is an assignment or...
    if (canAssign && match(TOKEN_EQUAL)) {
        expression(); // parse the right side
        emitBytes(setOp, (uint8_t)arg);
    } else { // ...just reading/accessing the variable
        emitBytes(getOp, (uint8_t)arg);
    }

}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // compile the operand
    parsePrecedence(PREC_UNARY);

    // emit the operator instruction.
    switch (operatorType) {
        case TOKEN_BANG:  emitByte(OP_NOT); break;
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
static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}



// ----------------------------------------------------------------------------
bool compile(const char* source, Chunk* chunk) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler);

    compilingChunk = chunk;

    // init parser
    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    endCompiler();
    return !parser.hadError;
}
