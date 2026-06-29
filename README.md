# Joy

A small Haskell interpreter for the [Joy programming language](https://hypercubed.github.io/joy/html/j02maf.html).

Joy is a functional, concatenative, stack-based language. Programs are built by placing words next to each other. Each word transforms the stack.

```joy
5 dup *      # 25
[1 2 3] [dup *] map
```

## Status

This is an experimental interpreter, not a complete Joy system.

It is useful for:

- learning how concatenative languages work
- experimenting with quotations and combinators
- using Joy examples in tests or small scripts
- hacking on a compact Haskell interpreter

It currently supports literals, quotations, stack words, arithmetic, comparisons, booleans, list and string operations, higher-order combinators, recursion combinators, user definitions, a REPL, and a small public Haskell API.

## Requirements

- GHC 9.6 or 9.8
- `cabal-install`
- `make`, optional

The GitHub Actions build tests GHC 9.6 and 9.8. Cabal is the supported build path.

If you do not have GHC and Cabal installed, use [GHCup](https://www.haskell.org/ghcup/).

## Setup

```bash
git clone https://github.com/owainlewis/joy.git
cd joy
cabal update
cabal v2-build all
cabal v2-test all --test-show-details=direct
```

You can also use the Makefile:

```bash
make build
make test
make run
```

## Running Joy

Start the REPL:

```bash
cabal v2-run joy-exe
```

Run a Joy file:

```bash
cabal v2-run joy-exe -- examples/factorial.joy
```

Run a short expression:

```bash
cabal v2-run joy-exe -- 1 2 +
```

For shell-sensitive words like `*`, prefer the REPL or a `.joy` file.

## REPL Commands

```text
:help          Show help
:load <file>   Load and execute a Joy file
:env           Show defined words
:clear         Clear all definitions
:quit, :q      Exit
```

Definitions entered in the REPL stay available until `:clear` or exit.

## Stack Display

The Haskell API stores the top of the stack at the head of the list.

The CLI prints the stack in reading order, from bottom to top:

```joy
joy> 1 2 swap
=> 2 1
```

The same result through `runJoy` is:

```haskell
Right [JInt 1, JInt 2]
```

## Quick Examples

Arithmetic:

```joy
2 3 +       # 5
10 4 /      # 2.5
5 dup *     # 25
```

Lists and strings:

```joy
1 [2 3] cons             # [1 2 3]
[1 2 3] first            # 1
[1 2 3] rest             # [2 3]
[1 2 3] reverse          # [3 2 1]
"hello" " world" concat  # "hello world"
```

Quotations:

```joy
5 [dup *] i              # 25
1 2 [10 +] dip           # 11 2
```

Higher-order combinators:

```joy
[1 2 3 4] [dup *] map    # [1 4 9 16]
[1 2 3 4] [2 >] filter   # [3 4]
[1 2 3 4] 0 [+] fold     # 10
[1 2 3] [dup *] step     # 1 4 9
1 5 [2 *] times          # 32
```

Conditionals:

```joy
5 [0 >] [pop 1] [pop -1] ifte  # 1
true [1] [2] branch            # 1
false 10 20 choice             # 20
```

Definitions:

```joy
[dup *] square define
5 square

DEFINE
  square == dup * ;
  quad == square square .

2 quad
```

Recursion:

```joy
5 [0 =] [pop 1] [dup 1 -] [*] linrec  # 120
```

## Public API

```haskell
import Language.Joy
import Language.Joy.VirtualMachine (Joy(..))

runJoy "1 2 +" == Right [JInt 3]
```

Main entry points:

- `runJoy :: String -> Either String Stack`
- `evalJoy :: String -> IO (Either VMError Stack)`
- `runJoyFile :: FilePath -> IO (Either String Stack)`
- `parseJoy :: String -> Either String [AST.Joy]`

The API returns stack values with the top of the stack first.

## Project Layout

```text
src/Language/Joy/
  Joy.hs              Public API and AST to VM transform
  AST.hs              Parser AST
  Lexer.hs            Parsec lexer
  Parser.hs           Joy parser
  Core.hs             Compatibility types and re-exports
  VirtualMachine.hs   Stack VM and primitives

app/Main.hs           CLI and REPL
test/Language/Joy/    Parser, VM, and integration tests
examples/             Runnable Joy programs
docs/JoyLanguage.md   Language notes for this interpreter
```

## Development

Build:

```bash
cabal v2-build all
```

Test:

```bash
cabal v2-test all --test-show-details=direct
```

Open a REPL for the library:

```bash
cabal v2-repl joy
```

Run formatting and whitespace checks before opening a PR:

```bash
git diff --check
```

GitHub Actions runs Cabal build and test jobs for GHC 9.6 and 9.8.

## Primitive Reference

Stack:

`dup`, `pop`, `swap`, `rollup`, `rolldown`, `rotate`, `dupd`, `swapd`, `popd`, `stack`, `unstack`, `newstack`, `id`

Arithmetic:

`+`, `-`, `*`, `/`, `%`, `div`, `rem`, `abs`, `neg`, `sign`, `max`, `min`, `succ`, `pred`

Comparison:

`<`, `>`, `<=`, `>=`, `=`, `!=`, `<>`

Boolean:

`and`, `or`, `not`, `xor`

Lists and strings:

`cons`, `swons`, `first`, `rest`, `uncons`, `unswons`, `concat`, `size`, `null`, `small`, `reverse`, `at`, `of`, `drop`, `take`

Quotation execution:

`i`, `x`, `dip`, `dipd`, `dipdd`, `app1`, `app2`, `nullary`, `unary`, `binary`, `ternary`

Conditionals:

`ifte`, `cond`, `choice`, `branch`

Higher-order:

`map`, `filter`, `fold`, `step`, `split`, `times`

Recursion:

`linrec`, `primrec`, `tailrec`, `genrec`, `binrec`

Type predicates:

`integer?`, `float?`, `number?`, `char?`, `string?`, `list?`, `leaf?`, `logical?`

Conversion:

`ord`, `chr`, `strtol`

Miscellaneous:

`unit`, `pair`, `unpair`, `infra`, `cleave`, `define`

## Known Limits

- Cabal is supported. Stack is not maintained for this repo.
- This is not a full implementation of the historical Joy language.
- IO words such as `put`, `putch`, and `print` are placeholders in the pure VM.
- `:load` executes a file and prints the result, but it does not import definitions into the current REPL session.

## License

MIT
