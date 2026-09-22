# CIS 194: Functional Programming in Haskell

Algorithmic implementations and data structure design for the University of Pennsylvania's CIS 194 Haskell curriculum. 

## Core Technical Concepts Demonstrated

*   **Algebraic Data Types & Immutable State:** Built custom ADTs and recursive Binary Search Trees (`MessageTree`) to parse, filter, and chronologically sort system logs without mutating state.
*   **Infinite Data Structures & Lazy Evaluation:** Implemented a custom `Stream` data type to mathematically model infinite sequences. Overloaded the `Num` and `Fractional` type classes to calculate the Fibonacci sequence via polynomial division and generating functions ($x / (1 - x - x^2)$).
*   **Type Classes & Polymorphism:** Designed abstract Syntax Trees (ASTs) and polymorphic evaluators using custom `Expr` type classes. Extended mathematical operations seamlessly to custom domains like modular arithmetic (`Mod7`) and Boolean logic bounds (`MinMax`).
*   **Higher-Order Functions & Folds:** Replaced standard recursion with advanced applications of `foldr`, `map`, and `filter` to generate balanced binary trees, build custom `xor` logic, and optimize mathematical sieves.

## Repository Structure & Execution

Each assignment is isolated within its own directory to provide structured locations for compiled Haskell files and independent testing environments. 

To run or test a specific module, load the file into the Glasgow Haskell Compiler interactive environment (GHCi):

```bash
cd Assignment_6
ghci Assignment6.hs
