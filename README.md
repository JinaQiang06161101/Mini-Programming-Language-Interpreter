# Mini-Programming-Language-Interpreter

## Overall
This project implements a simple interpreter for a custom programming language, written entirely in OCaml. It demonstrates fundamental concepts of language parsing, evaluation, and execution in a functional programming style.

## Features
- **Lexer & Parser**: Tokenizes and parses source code into an abstract syntax tree (AST).
- **Expression Evaluation**: Supports basic arithmetic operations, variables, and control flow.
- **Error Handling**: Provides informative error messages for syntax and runtime errors.
- **Extensible Design**: Modular code structure makes it easy to add new language features.

## Getting Started

### Prerequisites
- OCaml compiler (version 4.10 or above recommended)
- OPAM package manager (optional but recommended)

### Installation
Clone the repository:

```
git clone https://github.com/JQ0601/ocaml-language-interpreter.git
cd ocaml-language-interpreter
```

## Build the project in VSCode:
To compile the code, open the terminal in VS Code, cd to correct directory with your .ml code file, and type:

```
ocamlc -o <executable> <filename.ml>
```
To run the executable file write the following in the terminal:
```
./<executable>
```
