# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Run Commands

```bash
# Build the project
cabal v2-build all

# Run the REPL
cabal v2-run joy-exe

# Run tests
cabal v2-test all --test-show-details=direct

# Run a specific test suite
cabal v2-test joy-test --test-options='--match VMSpec'
cabal v2-test joy-test --test-options='--match ParserSpec'
cabal v2-test joy-test --test-options='--match IntegrationSpec'

# Execute a Joy program file
cabal v2-run joy-exe -- examples/factorial.joy

# Run Joy expression from command line
cabal v2-run joy-exe -- 1 2 +
```

## Project Structure

```
src/Language/Joy/
├── Joy.hs              # Main API (runJoy, evalJoy, runJoyFile)
├── AST.hs              # Parser AST types (Lit, Joy)
├── Lexer.hs            # Lexical analysis (Parsec-based)
├── Parser.hs           # Joy parser
├── Core.hs             # Re-exports from VM + legacy types
└── VirtualMachine.hs   # Stack VM with all primitives (~1400 lines)

app/
└── Main.hs             # REPL and CLI interface

test/Language/Joy/
├── ParserSpec.hs       # Parser unit tests
├── VMSpec.hs           # VM unit tests (70+ tests)
└── IntegrationSpec.hs  # End-to-end tests

examples/
├── hello.joy           # Basic example
├── basics.joy          # Arithmetic and stack ops
├── factorial.joy       # Recursion with linrec
├── higher_order.joy    # map, filter, fold examples
└── definitions.joy     # User-defined words
```

## Key Components

### 1. Virtual Machine (`VirtualMachine.hs`)

The heart of the interpreter. Key types:

```haskell
data Joy = JInt Integer | JFloat Double | JBool Bool
         | JChar Char | JString Text | JWord Text | JQuote [Joy]

data VMState = VMState { vmStack :: Stack, vmEnv :: Env }
type VM a = StateT VMState (Except VMError) a

runProgram :: [Joy] -> Either VMError Stack
```

~90 primitives implemented in categories:
- **Stack**: dup, pop, swap, rollup, rolldown, rotate, dupd, swapd, popd
- **Arithmetic**: +, -, *, /, %, div, abs, neg, succ, pred, max, min
- **Comparison**: <, >, <=, >=, =, !=
- **Boolean**: and, or, not, xor
- **List**: cons, first, rest, uncons, concat, size, null, reverse, at, take, drop
- **Combinators**: i, x, dip, dipd, app1, app2, nullary, unary, binary
- **Conditionals**: ifte, cond, choice, branch
- **Higher-order**: map, filter, fold, step, split, times
- **Recursion**: linrec, primrec, tailrec, genrec, binrec
- **Type predicates**: integer?, float?, list?, string?, etc.
- **Definition**: define (stores word in environment)

### 2. Parser (`Parser.hs`)

Parsec-based parser. Supports:
- Literals: integers, floats, booleans, strings, chars
- Symbolic operators: +, -, *, /, <, >, <=, >=, =, !=
- Lists/quotations: `[1 2 3]`, `[[nested]]`
- Definitions: `foo == 1 2 +` and `DEFINE foo == ... ; bar == ... .`
- Comments: `# line comment`

### 3. Main Module (`Joy.hs`)

Public API:

```haskell
runJoy :: String -> Either String Stack      -- Parse and run
evalJoy :: String -> IO (Either VMError Stack)  -- IO version
runJoyFile :: FilePath -> IO (Either String Stack)
parseJoy :: String -> Either String [AST.Joy]
```

### 4. AST (`AST.hs`)

Parser output types (distinct from VM Joy type):

```haskell
data Lit = Boolean Bool | Char Char | Integer Integer | Float Double
         | String String | Identifier String

data Joy = Literal Lit | List [Joy] | Definition String [Joy] | DefinitionList [Joy]
```

## Architecture Flow

```
Source Code → Parser → AST.Joy → Transform → VM.Joy → Evaluate → Stack Result
```

1. **Parse**: Text → `[AST.Joy]` via Parsec
2. **Transform**: `AST.Joy` → `[VM.Joy]` (flattens, converts identifiers to `JWord`)
3. **Evaluate**: `[VM.Joy]` → execute on stack VM

## Joy Language Quick Reference

```joy
# Arithmetic
1 2 +                    # => 3

# Stack ops
5 dup *                  # => 25 (square)

# Lists
[1 2 3] first            # => 1
[1 2 3] rest             # => [2 3]
1 [2 3] cons             # => [1 2 3]

# Quotation execution
5 [dup *] i              # => 25

# Higher-order
[1 2 3] [dup *] map      # => [1 4 9]
[1 2 3 4] [2 >] filter   # => [3 4]
[1 2 3] 0 [+] fold       # => 6

# Conditionals
5 [0 >] [pop 1] [pop -1] ifte    # => 1

# Recursion (factorial)
5 [0 =] [pop 1] [dup 1 -] [*] linrec  # => 120

# Definitions
[dup *] square define
5 square                 # => 25
```

## Testing

Tests are in `test/Language/Joy/`:
- `VMSpec.hs`: 70+ tests covering all VM operations
- `ParserSpec.hs`: Parser tests including symbolic operators
- `IntegrationSpec.hs`: End-to-end source-to-result tests

Run all tests:
```bash
cabal v2-test all --test-show-details=direct
```

## Common Development Tasks

### Adding a new primitive

1. Add the operation to `primitives` map in `VirtualMachine.hs`
2. Implement `opXxx :: VM ()` function
3. Add tests in `VMSpec.hs`

### Modifying the parser

1. Edit `Parser.hs` (uses Parsec combinators)
2. Update `Lexer.hs` if new tokens needed
3. Add tests in `ParserSpec.hs`

### Debugging

The REPL shows the result stack after each command. It persists definitions, not the data stack:
```
joy> 1 2 3 +
=> 1 5
```

Use `:help` in REPL for commands.
