# REHyper
REHyper is a runtime enforcement tool for temporal hyperproperties given as universal HyperLTL formulas.
This repository is the official implementation of the ATVA'21 paper "Runtime Enforcement of Hyperproperties".

REHyper monitors a reactive system for undesired behavior at runtime and corrects the system's output in case it violates the given hyperproperty specification.
The implementation supports two input models.
The parallel trace input model, where the number of traces is known a-priori and all traces are processed in parallel.
The sequential trace input model, where traces are processed sequentially and no a-priori bound on the number of traces is known.

## Dependencies:
* [Rust/Cargo](https://rustup.rs/) 1.45.0.
* [spot](https://spot.lrde.epita.fr/).
* C++ compiler supporting C++17 and OpenMP, e.g. [GCC](https://gcc.gnu.org/) 8 and later.
* [GNU Make](https://www.gnu.org/software/make/).
* [Boost](http://www.boost.org/) 1.53 or higher with the following libraries: program options, filesystem, iostreams and system.
* [zlib](http://www.zlib.net/).
* [CMake](https://cmake.org/) 3.11 or higher.
* JDK 12, either from [Oracle](http://www.oracle.com/technetwork/java/javase/downloads/index.html) or [OpenJDK](http://openjdk.java.net/projects/jdk/12/).
* [PGSolver](https://github.com/tcsprojects/pgsolver/)

Optional dependencies:
- [EAHyper](https://github.com/reactive-systems/eahyper) (recommended): Provide as executable named `eahyper` in working directory when executing enforcer.

## HyperLTL
HyperLTL syntax:

    HyperLTL ::=
        "forall" Identifier "." HyperLTL
      | "exists" Identifier "." HyperLTL
      | LTL

    LTL ::=
        Identifier "_" Identifier
      | "true" | "false"

        (* unary operators *)
      | ("!" | "~") LTL | "X" LTL | "F" LTL | "G" LTL

        (* binary operators operators *)
      | LTL "U" LTL | LTL "W" LTL | LTL "R" LTL (* precedence 0, right associative *)
      | LTL ("<->" | "<=>") LTL                 (* precedence 1, left associative *)
      | LTL ("->" | "=>") LTL                   (* precedence 2, right associative *)
      | LTL "|" LTL                             (* precedence 3, left associative *)
      | LTL "^" LTL                             (* precedence 4, left associative *)
      | LTL "&" LTL                             (* precedence 5, left associative *)

    Identifier ::=
        [a-zA-Z][a-zA-Z0-9]*
       | '"' [^"]* '"'
       | "'" [^']* "'"

## Traces
Trace files contain one or more event per line, separated by '|'. An event is a list of atomic propositions seperated by ','.

For parallel monitoring, REHyper is given either one file per trace or one file containing all traces. Example:

    trace_1.tr:
        a,b
        a
        b,c

    trace_2.tr:
        a
        a
        a,b

REHyper can either be called with both files or one file with the following content:

    a,b|a
    a|a
    b,c|a,b

For sequential monitoring, either one file per trace is given or one file containing all traces. In this case, the traces are separated by a line containing ";;":

    a,b
    a
    b,c
    ;;
    a
    a
    a,b

## Parallel monitoring
To enforce multiple traces given in the files input\_1.tr, ..., input\_n.tr in parallel, use:

    rehyper -f FORMULA [-P] input_1.tr ... input_n.tr [-o output_1.tr ... output_n.tr]

If no output files are given, all traces will be written to stdout, separated by '|'.
To read a single file containing all traces separated by '|', the number of traces must be given explicitly:

    rehyper -F FORMULA -X N [input.tr] [-o output_1.tr ... output_n.tr]

If no input file is given, REHyper reads the traces from stdin.

## Sequential monitoring
To enforce multiple traces given in the files input\_1.tr, ..., input\_n.tr sequentially, use:

    rehyper -f FORMULA -S [input_1.tr ... input_n.tr] [-o output_1.tr ... output_n.tr]

If no output files are given, all traces will be written to stdout, separated by ';;'.
Each input file may also contain multiple traces seperated by ';;'.
If no input files are given, REHyper reads the traces from stdin.

## Reactive systems
REHyper assumes a reactive system as soon as inputs are specified:

    rehyper -f FORMULA -i INPUTS ...

INPUTS is a comma-separated list of atomic propositions.
