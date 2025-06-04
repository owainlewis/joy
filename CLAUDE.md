# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Run Commands

```bash
# Build the project
stack build

# Run the REPL
stack run

# Run tests
stack test

# Run a specific test
stack test --test-arguments "-m \"ParserSpec\""

# Execute a Joy program file
stack run < examples/hello.joy
```

## Project Structure

The Joy project is an implementation of the Joy programming language in Haskell. Joy is a concatenative, stack-based programming language.

### Key Components

1. **AST (Abstract Syntax Tree)** - `src/Language/Joy/AST.hs`
   - Defines the core data types for the Joy language
   - `Lit` type for literals (Boolean, Char, Integer, Float, String, Identifier)
   - `Joy` type for language constructs (Literal, List, Definition, DefinitionList)

2. **Parser** - `src/Language/Joy/Parser.hs` and `src/Language/Joy/Lexer.hs`
   - Parses Joy source code into AST
   - Uses Parsec for lexing and parsing
   - Handles various Joy syntax elements (literals, lists, definitions)

3. **Core** - `src/Language/Joy/Core.hs`
   - Contains basic operations and data types
   - Defines program errors and core functions

4. **Virtual Machine** - `src/Language/Joy/VirtualMachine.hs`
   - Stack-based virtual machine for executing Joy programs
   - Provides instruction set (Push, Pop, Apply, Print)
   - Implements evaluation of Joy programs

5. **Main Module** - `src/Language/Joy.hs`
   - Provides the main entry point for the library
   - Exposes the `runJoy` function for executing Joy code

### Architecture

Joy follows a typical interpreter architecture:

1. **Lexing/Parsing**: Source code is tokenized and parsed into an AST
2. **Evaluation**: The AST is evaluated on a stack-based virtual machine
3. **Execution**: Instructions modify the stack and environment to produce results

The project uses a monadic approach for handling state and errors during execution, with a state monad for managing the VM state and an error monad for handling runtime errors.

## Joy Language Overview

Joy is a concatenative, stack-based language. Key characteristics:

- Programs operate on a stack of values
- Function composition is done by concatenation
- Quotations (code blocks) are used for delayed execution
- Uses postfix notation

Common operations include:
- Stack manipulation: dup, swap, pop
- Combinators: i (execute quotation), dip (execute under), etc.
- List operations: cons, first, rest
- Recursive definitions and conditionals

Examples of Joy code can be found in the `examples/` directory.