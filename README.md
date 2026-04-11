# LFAC – Custom Programming Language

A custom programming language built with **Flex** (lexer) and **Bison** (parser) as a university project for the *Formal Languages, Automata and Compilers* (LFAC) course.

The language uses unconventional, prefix-style syntax (keywords are prefixed with `!`) and supports variables, constants, arrays, classes, functions, control flow, arithmetic, and boolean expressions.

---

## Features

| Feature | Description |
|---|---|
| **Types** | `int`, `float`, `char`, `string`, `bool` |
| **Constants** | `!const` modifier prevents reassignment |
| **Arrays** | Integer arrays and mixed-type string arrays |
| **Classes** | User-defined types with fields and methods |
| **Functions** | Named functions with typed parameters |
| **Control flow** | `!if`, `!while`, `!for` |
| **Arithmetic** | `+++` (add), `---` (sub), `***` (mul), `///` (div) |
| **Boolean ops** | `!&&`, `!||`, `!>`, `!<`, `!==` |
| **String ops** | `!strcpy`, `!strcat`, `!strlength` |
| **Eval** | `!Eval` prints the result of an expression at runtime |
| **Comments** | `/* ... */` block comments |

---

## Syntax Overview

### Variables

Variables are prefixed with `^` and declared with the type keyword followed by `<<`.

```
!int<<^Ia<-16;
!float<<^Fa<-!F 12,5;
!char<<^Ca<-!C s;
!string<<^Sa<-!strcpy hello;
!bool<<^Ba<-!B YES;
```

> Floating-point literals use a comma as the decimal separator: `12,5` means `12.5`.

### Constants

```
!const !int<<^MaxVal<-100;
```

### Arrays

Integer arrays use `#` delimiters and `*` as element separator:

```
!int<<^Id<-# 10 * 20 * 30 #;
^Id[1]<-99;
```

### Classes

```
!class<<^Point#
    !int<<^X;
    !int<<^Y;
#
```

Access class members with `.`:

```
^Point.^X<-5;
```

### Functions

```
!definefunction !^Add <(!int<<^A; !int<<^B;)> %
    !int<<^Result<-^A +++ ^B;
%
```

Call a function:

```
<()> !^Add ; 3 ; 7 <()>
```

### Control Flow

```
!if<<!B ^Ba>>%
    ^Ia<-^Ia +++ 1;
%

!while<<!B ^Ba>>%
    ^Ia<-^Ia +++ 1;
%

!for<<!int<<^I<-0; !B ^I !< 10 ; ^I<-^I +++ 1;>>%
    ^Ia<-^Ia +++ 1;
%
```

### Eval

Print the result of an expression:

```
!Eval ^Ia +++ 5;
```

---

## Build

Requires **flex**, **bison**, and **gcc**.

```bash
bison -d mask.y          # generates y.tab.c and y.tab.h
flex 1.l                  # generates lex.yy.c
gcc y.tab.c lex.yy.c -o lfac -lfl
```

### Run

```bash
./lfac < P.txt
```

Replace `P.txt` with any source file written in this language.

---

## Example Program

The file `P.txt` demonstrates the full feature set:

```
!definefunction !^Func <(!int<<^Funa; !bool<<^Funb; !string<<^Func;)> %
    !int<<^Fia<-32;
%

!int<<^Ia<-16;
!int<<^Ib<-32;

!bool<<^Ba<-!B YES;
!float<<^Fa<-!F 123,12;
!string<<^Sa<-!strcpy hello;
!char<<^Ca<-!C s;

!for<<;!B ^Ia !> ^Ib ;>>%
    ^Ia<-^Ia +++ 2;
%

!Eval ^Ia;
```

---

## Error Reporting

The interpreter reports semantic errors with line numbers, for example:

- Redeclaring an already-declared variable
- Using an undeclared variable
- Type mismatch on assignment
- Calling a function with the wrong number or type of arguments
- Modifying a constant

---

## Project Structure

| File | Description |
|---|---|
| `1.l` | Flex lexer — tokenises the source language |
| `mask.y` | Bison grammar — parser and semantic actions |
| `P.txt` | Sample program demonstrating all features |
| `de retinut.txt` | Language reference with C equivalents |
| `orori.txt` | Error test cases |
