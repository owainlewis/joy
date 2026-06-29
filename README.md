# Joy

A Haskell implementation of the [Joy programming language](https://hypercubed.github.io/joy/html/j02maf.html), a purely functional, concatenative, stack-based language.

## What is Joy?

Joy is a functional programming language designed by Manfred von Thun. Unlike most languages, Joy uses **postfix notation** and **function composition through concatenation**. Programs are sequences of operations that transform a stack of values.

```joy
# Traditional: square(5) or 5.square()
# Joy: 5 dup *

5 dup *    # Push 5, duplicate it, multiply => 25
```

Key characteristics:
- **Stack-based**: All operations consume and produce values on a stack
- **Concatenative**: Programs are composed by concatenation (juxtaposition)
- **Quotations**: Code blocks `[...]` are first-class values
- **Combinator-rich**: Powerful combinators like `map`, `fold`, `ifte`, `linrec`

## Quick Start

### Building

```bash
# Build the project
cabal v2-build all

# Run tests
cabal v2-test all

# Start the REPL
cabal v2-run joy-exe
```

### Interactive REPL

```
$ cabal v2-run joy-exe
Joy Interpreter v0.2.0
Type :help for help, :quit to exit

joy> 1 2 +
=> 3

joy> [1 2 3 4 5] [dup *] map
=> [1 4 9 16 25]

joy> 5 [0 =] [pop 1] [dup 1 -] [*] linrec
=> 120
```

### Running Files

```bash
cabal v2-run joy-exe -- examples/factorial.joy
```

## Language Overview

### Literals

```joy
42          # Integer
3.14        # Float
true false  # Booleans
'a'         # Character
"hello"     # String
[1 2 3]     # List/Quotation
```

### Stack Operations

| Operation | Stack Effect | Description |
|-----------|--------------|-------------|
| `dup`     | `X → X X` | Duplicate top |
| `pop`     | `X →` | Remove top |
| `swap`    | `X Y → Y X` | Swap top two |
| `rollup`  | `X Y Z → Z X Y` | Rotate three up |
| `rolldown`| `X Y Z → Y Z X` | Rotate three down |
| `rotate`  | `X Y Z → Z Y X` | Reverse three |

### Arithmetic

```joy
2 3 +       # => 5
10 3 -      # => 7
4 5 *       # => 20
10 4 /      # => 2.5
10 3 div    # => 3 (integer division)
10 3 %      # => 1 (modulo)
-5 abs      # => 5
5 neg       # => -5
```

### Comparison & Boolean

```joy
3 5 <       # => true
5 5 =       # => true
3 5 !=      # => true

true false and   # => false
true false or    # => true
true not         # => false
```

### List Operations

```joy
1 [2 3] cons      # => [1 2 3]
[1 2 3] first     # => 1
[1 2 3] rest      # => [2 3]
[1 2 3] uncons    # => 1 [2 3]
[1 2] [3 4] concat # => [1 2 3 4]
[1 2 3] size      # => 3
[1 2 3] reverse   # => [3 2 1]
[] null           # => true
```

### Quotation Execution

Quotations are code blocks that can be manipulated as data and executed:

```joy
# i - execute a quotation
5 [dup *] i       # => 25

# dip - execute quotation under the top value
1 2 [10 +] dip    # => 2 11

# x - duplicate quotation and execute
[dup *] x         # => executes [dup *] with itself on stack
```

### Conditionals

```joy
# ifte - if-then-else
5 [0 >] [1] [-1] ifte     # => 1 (5 > 0, so execute [1])

# branch - boolean dispatch
true [1] [2] branch       # => 1

# choice - select value
true 10 20 choice         # => 10
```

### Higher-Order Combinators

```joy
# map - apply to each element
[1 2 3] [dup *] map           # => [1 4 9]

# filter - select matching elements
[1 2 3 4 5] [2 >] filter      # => [3 4 5]

# fold - reduce list
[1 2 3 4] 0 [+] fold          # => 10

# step - apply to each, leave results on stack
[1 2 3] [dup *] step          # => 1 4 9

# times - repeat N times
1 5 [2 *] times               # => 32
```

### Recursion Combinators

Joy provides powerful recursion combinators that eliminate explicit recursion:

```joy
# linrec - linear recursion
# [test] [base] [rec1] [rec2] linrec
# If test is true, execute base. Otherwise: rec1, recurse, rec2

# Factorial: 5! = 120
5 [0 =] [pop 1] [dup 1 -] [*] linrec

# primrec - primitive recursion
# Operates on integers or lists with base case and combiner

# binrec - binary recursion (divide and conquer)
# Useful for algorithms like quicksort
```

### User Definitions

Define new words using the `define` operation:

```joy
# Define square
[dup *] square define
5 square                  # => 25

# Define using other definitions
[square square] quad define
2 quad                    # => 16

# Define factorial
[[0 =] [pop 1] [dup 1 -] [*] linrec] factorial define
5 factorial               # => 120
```

### Type Predicates

```joy
5 integer?      # => true 5
3.14 float?     # => true 3.14
[1 2] list?     # => true [1 2]
"hi" string?    # => true "hi"
```

## Examples

### Factorial

```joy
# Using linrec
5 [0 =] [pop 1] [dup 1 -] [*] linrec
# => 120
```

### Fibonacci

```joy
# nth Fibonacci number using binrec
10 [2 <] [] [dup 1 - swap 2 -] [+] binrec
# => 55
```

### Sum of Squares

```joy
[1 2 3 4 5] [dup *] map 0 [+] fold
# => 55
```

### Quicksort

```joy
# Using binrec for divide-and-conquer
[small] [] [uncons [>] split] [swapd concat cons concat] binrec
```

### Filter and Transform

```joy
# Get squares of even numbers from 1-10
[1 2 3 4 5 6 7 8 9 10]
  [2 % 0 =] filter      # Keep evens: [2 4 6 8 10]
  [dup *] map           # Square them: [4 16 36 64 100]
```

## Architecture

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Source    │────▶│   Parser    │────▶│     AST     │
│    Code     │     │  (Parsec)   │     │             │
└─────────────┘     └─────────────┘     └─────────────┘
                                              │
                                              ▼
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Result    │◀────│  Evaluator  │◀────│  Transform  │
│   Stack     │     │    (VM)     │     │  AST → Joy  │
└─────────────┘     └─────────────┘     └─────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `Language.Joy` | Main API: `runJoy`, `evalJoy`, `runJoyFile` |
| `Language.Joy.VirtualMachine` | Stack-based VM with all primitives |
| `Language.Joy.Parser` | Parsec-based Joy parser |
| `Language.Joy.Lexer` | Lexical analysis |
| `Language.Joy.AST` | Abstract syntax tree types |
| `Language.Joy.Core` | Core types (re-exports from VM) |

## Development

Build everything:

```bash
cabal v2-build all
```

Run the test suite:

```bash
cabal v2-test all --test-show-details=direct
```

Run the interpreter:

```bash
cabal v2-run joy-exe
```

Run a source file:

```bash
cabal v2-run joy-exe -- examples/factorial.joy
```

GitHub Actions runs the Cabal build and test suite on GHC 9.6 and 9.8.

## Complete Primitive Reference

### Stack Manipulation
`dup`, `pop`, `swap`, `rollup`, `rolldown`, `rotate`, `dupd`, `swapd`, `popd`, `stack`, `unstack`, `newstack`, `id`

### Arithmetic
`+`, `-`, `*`, `/`, `%`, `div`, `rem`, `abs`, `neg`, `sign`, `max`, `min`, `succ`, `pred`

### Comparison
`<`, `>`, `<=`, `>=`, `=`, `!=`, `<>`

### Boolean
`and`, `or`, `not`, `xor`

### List Operations
`cons`, `swons`, `first`, `rest`, `uncons`, `unswons`, `concat`, `size`, `null`, `small`, `reverse`, `at`, `of`, `drop`, `take`

### Quotation Execution
`i`, `x`, `dip`, `dipd`, `dipdd`, `app1`, `app2`, `nullary`, `unary`, `binary`, `ternary`

### Conditionals
`ifte`, `cond`, `choice`, `branch`

### Higher-Order
`map`, `filter`, `fold`, `step`, `split`, `times`

### Recursion
`linrec`, `primrec`, `tailrec`, `genrec`, `binrec`

### Type Predicates
`integer?`, `float?`, `number?`, `char?`, `string?`, `list?`, `leaf?`, `logical?`

### Type Conversion
`ord`, `chr`, `strtol`

### Miscellaneous
`unit`, `pair`, `unpair`, `infra`, `cleave`, `define`

## Resources

- [Joy Language Homepage](https://hypercubed.github.io/joy/html/j02maf.html)
- [Mathematical Foundations of Joy](http://www.kevinalbrecht.com/code/joy-mirror/j02maf.html)
- [Joy Manual](http://www.kevinalbrecht.com/code/joy-mirror/plain-manual.html)
- [Concatenative Languages Wiki](https://concatenative.org/)

## License

MIT
