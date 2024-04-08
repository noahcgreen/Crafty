# Crafty Scheme

## Overview

**Crafty** is a Haskell implementation of [R7RS Scheme](https://r7rs.org). This project is my long-running playground for learning about:
- Haskell, Scheme, and functional programming in general
- Interpreter/compiler theory and practice

The aim is to produce a Scheme interpreter which can be embedded within Haskell
applications or run standalone.

The project's name is a reference to [Crafting Interpreters](https://craftinginterpreters.com).

## Roadmap

- Basic parser implemented with Parsec
- Tree-walk interpreter
- End-to-end eval test suite
- Garbage collector
- Rewrite parser with custom tokenizer + parser, eliminating Parsec dependency
- Replace tree-walk interpreter with a bytecode VM
- Bonus: Compiler to produce native executables?
