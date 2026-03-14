# (Near) Bit-Optimal Representations of ADT in Haskell

This is an experimental Haskell project exploring **type-informed compression**, (near) bit-optimal runtime representations derived directly from Haskell types.

See [SuccinctDS.pdf](SuccinctDS.pdf) for documentation.

## Overview

We start by implementing succinct trees—data structures that provide almost information-theoretically optimal space bounds for tree representations. In future versions, we aim leverage the Haskell type system to automatically derive (near) bit-optimal representations for arbitrary algebraic data types. The ultimate goal is to offer a transparent interface that uses the most efficient bit-level layout and its corresponding operations, eliminating the need for complex manual bit manipulation.

In preliminary experiments, our succinct tree implementation, albeit in a beta stage, achieves significantly faster traversal times—often by one or more orders of magnitude—while using at least 2× less memory than a conventional pointer-based baseline.

## Building and Running
Compile the executable prototype and generate the LaTeX report with:
```
make all
```
Run the prototype with
```
stack run
```

The executable may prompt for a tree depth; larger depths amplify the performance and memory differences between the vanilla and succinct representations.

## Documentation

For a detailed description of the design, theory, and experimental results, see [SuccinctDS.pdf](SuccinctDS.pdf).

This document explains the DFUDS encoding, and the benchmark setup in more depth.

## Status

This is **research code**:

- API and implementation details may change.
- Some components are intentionally not fully optimized to keep the code understandable.
- The focus is on demonstrating feasibility and performance potential, not on providing a production-ready library.

## License

This project is licensed under the GNU General Public License (GPL), version 3.

See the `LICENSE` file for the full license text.
