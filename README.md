# MonCompilateur

A compiler that translates a **Pascal-like imperative language** into **64-bit x86 (80x86) assembly** in AT&T syntax. Written in C++, it implements the full front-to-back pipeline: lexical analysis, recursive-descent (LL(k)) parsing, and assembly code generation.

## What It Does

- **Lexical analysis** — a Flex-based tokenizer (`tokeniser.l`) turns source code into tokens.
- **Parsing** — a recursive-descent LL(k) parser validates the grammar of the input program.
- **Code generation** — emits 64-bit x86 assembly (AT&T syntax) that can be assembled and run.

## Tech Stack

- C++
- Flex (lexical analyzer generator)
- GNU toolchain (g++, gcc, as)

## Build & Run

```bash
# Build the compiler
g++ -ggdb compilateur.cpp -o compilateur

# Compile a source program to assembly
cat test.p | ./compilateur > test.s

# Assemble and link into an executable
gcc -ggdb test.s -o test

# Run it
./test
```

## Project Structure

- `compilateur.cpp` — parser and assembly code generator
- `tokeniser.l` / `tokeniser.cpp` — Flex lexical analyzer
- `test.p` — sample source program
- `test.s` — generated assembly output
- `makefile` — build automation

## Notes

Compilation course project, building a working compiler from scratch to understand the full path from source code to machine-level assembly.
