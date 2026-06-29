# Joy Language Notes

These notes describe the Joy subset implemented in this repository.

Joy programs transform a stack. Literals push values. Words either run built-in primitives or user-defined quotations.

## Values

```joy
42
3.14
true
false
'a'
"hello"
[1 2 3]
```

Square brackets create quotations. A quotation is both data and executable code.

## Comments

```joy
# Everything after # is a comment.
1 2 +
```

## Stack Order

Internally, the VM stores the top of the stack first.

The CLI prints stacks from bottom to top:

```joy
joy> 1 2 swap
=> 2 1
```

## Core Stack Words

```joy
5 dup       # 5 5
1 2 swap   # 2 1
1 2 pop    # 1
```

Other stack words include `rollup`, `rolldown`, `rotate`, `dupd`, `swapd`, `popd`, `stack`, `unstack`, `newstack`, and `id`.

## Arithmetic

```joy
2 3 +       # 5
5 2 -       # 3
4 5 *       # 20
20 4 /      # 5.0
10 3 div    # 3
10 3 %      # 1
```

`/` always returns a float. `div`, `rem`, and `%` are integer operations.

## Lists And Strings

```joy
1 [2 3] cons             # [1 2 3]
[1 2 3] first            # 1
[1 2 3] rest             # [2 3]
[1 2 3] reverse          # [3 2 1]
"hello" size             # 5
"hello" " world" concat  # "hello world"
```

List and string words include `concat`, `size`, `null`, `small`, `reverse`, `at`, `of`, `drop`, and `take`.

## Quotations

Use `i` to execute a quotation:

```joy
5 [dup *] i
```

Use `dip` to run a quotation under the top value:

```joy
1 2 [10 +] dip  # 11 2
```

Other quotation words include `x`, `dipd`, `dipdd`, `app1`, `app2`, `nullary`, `unary`, `binary`, and `ternary`.

## Conditionals

`ifte` takes three quotations: condition, then branch, else branch.

The condition runs against the current stack. The original stack is restored before the selected branch runs.

```joy
5 [0 >] [pop 1] [pop -1] ifte
```

`branch` dispatches on a boolean:

```joy
true [1] [2] branch
```

`choice` selects one of two values:

```joy
false 10 20 choice  # 20
```

## Higher-Order Words

```joy
[1 2 3] [dup *] map       # [1 4 9]
[1 2 3 4] [2 >] filter    # [3 4]
[1 2 3 4] 0 [+] fold      # 10
[1 2 3] [dup *] step      # 1 4 9
1 5 [2 *] times           # 32
```

`map`, `filter`, and `fold` restore the surrounding stack for each item. `step` leaves each result on the stack.

## Definitions

Inline definition:

```joy
[dup *] square define
5 square
```

Definition block:

```joy
DEFINE
  square == dup * ;
  quad == square square .

2 quad
```

Definitions are stored in the VM environment. In the REPL, definitions persist until `:clear` or exit.

## Recursion

Factorial with `linrec`:

```joy
5 [0 =] [pop 1] [dup 1 -] [*] linrec
```

Supported recursion words:

- `linrec`
- `primrec`
- `tailrec`
- `genrec`
- `binrec`

## Type Predicates

Predicates preserve the original value and push a boolean:

```joy
5 integer?      # 5 true
[1 2] list?     # [1 2] true
"hi" string?    # "hi" true
```

Supported predicates:

`integer?`, `float?`, `number?`, `char?`, `string?`, `list?`, `leaf?`, `logical?`

## Not Full Joy

This interpreter does not implement the full historical Joy language. If a word is not listed in the README primitive reference, treat it as unsupported.
