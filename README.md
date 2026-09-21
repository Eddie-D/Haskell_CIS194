# CIS 194: Functional Programming in Haskell

Algorithmic implementations and data structure design for the University of Pennsylvania's CIS 194 Haskell curriculum. 

This repository serves as a practical demonstration of functional programming paradigms. The code focuses on leveraging Haskell's strong type system, lazy evaluation, and pure functions to solve complex algorithmic and mathematical problems efficiently.

## Core Technical Concepts Demonstrated

*   **Algebraic Data Types & Immutable State:** Built custom ADTs and recursive Binary Search Trees (`MessageTree`) to parse, filter, and chronologically sort system logs without mutating state.
*   **Infinite Data Structures & Lazy Evaluation:** Implemented a custom `Stream` data type to mathematically model infinite sequences. Overloaded the `Num` and `Fractional` type classes to calculate the Fibonacci sequence via polynomial division and generating functions ($x / (1 - x - x^2)$).
*   **Type Classes & Polymorphism:** Designed abstract Syntax Trees (ASTs) and polymorphic evaluators using custom `Expr` type classes. Extended mathematical operations seamlessly to custom domains like modular arithmetic (`Mod7`) and Boolean logic bounds (`MinMax`).
*   **Higher-Order Functions & Folds:** Replaced standard recursion with advanced applications of `foldr`, `map`, and `filter` to generate balanced binary trees, build custom `xor` logic, and optimize mathematical sieves.

## Assignment Breakdown

*   **Assignment 1 (Credit Card Validation & Hanoi):** Recursive algorithmic logic for digit extraction, validation, and solving the Tower of Hanoi puzzle using tuple mapping.
*   **Assignment 2 (Log Analysis):** File parsing into Algebraic Data Types (`LogMessage`, `MessageType`), mapped iteratively into an immutable Binary Search Tree for chronological sorting and error extraction.
*   **Assignment 3 (Golf):** Concise list manipulation covering step-wise array skipping, local maxima detection via pattern matching, and dynamic string-based histogram generation.
*   **Assignment 4 (Higher-Order Programming):** Extensive use of folds to implement a balanced tree inserter (`foldTree`) and mathematical modeling of the Sieve of Sundaram for prime number generation.
*   **Assignment 5 (Calc):** Built a calculator and expression parser utilizing Haskell's type classes, allowing a generic string input to be evaluated polymorphically into different custom types (`Integer`, `Bool`, `MinMax`, `Mod7`).
*   **Assignment 6 (Streams & Generating Functions):** Modeled infinite `Stream` data structures from scratch. Implemented stream mapping, zipping, and mathematical instance overloading to perform lazy linear-time generation of mathematical sequences.

## Repository Structure & Execution

Each assignment is isolated within its own directory to provide structured locations for compiled Haskell files and independent testing environments. 

To run or test a specific module, load the file into the Glasgow Haskell Compiler interactive environment (GHCi):

```bash
cd Assignment_6
ghci Assignment6.hs
