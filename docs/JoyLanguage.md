# The Joy Programming Language

Joy is a purely functional programming language created by Manfred von Thun that is based on composition of functions rather than application. It is a member of the family of concatenative programming languages.

## Core Concepts

Joy is stack-based, meaning that all operations manipulate a stack of values. Unlike most programming languages, Joy uses postfix notation, where operators come after their operands.

### Stack Manipulation

The basic stack operations in Joy:

- `dup`: Duplicates the top element of the stack: `[X | S] => [X X | S]`
- `swap`: Swaps the top two elements of the stack: `[X Y | S] => [Y X | S]`
- `pop`: Removes the top element from the stack: `[X | S] => [S]`
- `stack`: Pushes the entire stack onto itself: `[S] => [S | S]`

### Quotations

Joy uses quotations (enclosed in square brackets) to represent code that can be passed around as data:

```
[2 +]   # A quotation that adds 2 to a number
```

### Combinators

Combinators are functions that operate on quotations:

- `i`: Executes a quotation: `[[P] | S] => execute P on S`
- `dip`: Executes a quotation after temporarily removing the top element: `[X [P] | S] => [X | execute P on S]`
- `app2`: Applies a quotation to two elements: `[[P] X Y | S] => [P(X) P(Y) | S]`
- `map`: Applies a quotation to each element of a list

### Defining Functions

Joy allows definition of new functions:

```
DEFINE
    square == dup * ;    # Define a function that squares a number
    cube == dup dup * * .  # Define a function that cubes a number
```

## Examples

### Simple Arithmetic

```
2 3 +     # Result: 5
5 2 -     # Result: 3
4 5 *     # Result: 20
20 4 /    # Result: 5
```

### Working with Lists

```
[1 2 3] [4 5 6] concat   # Result: [1 2 3 4 5 6]
[1 2 3] first            # Result: 1
[1 2 3] rest             # Result: [2 3]
```

### Using Combinators

```
5 [2 +] i        # Result: 7
5 6 [*] i        # Result: 30
5 [2 *] [3 +] compose i  # Result: 13
```

## Implemented Features

This implementation of Joy currently supports:

- Basic literal types: integers, floats, booleans, strings, characters
- Stack manipulation: dup, swap, pop
- List operations: cons, first, rest
- Basic combinators: i, dip
- Definition syntax

## Future Enhancements

- Additional arithmetic operations (+, -, *, /, etc.)
- Full implementation of combinators (map, filter, etc.)
- Library of standard functions
- Module system
- Better error handling and debugging