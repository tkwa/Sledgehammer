# Sledgehammer

Sledgehammer is a code-golf language defined by a compressor and golfing library for Wolfram Language. Its goal is to minimize size of compressed source.

### Prior art

While [Mthmtca](https://github.com/LegionMammal978/Mthmtca) compresses each Mathematica builtin into 4 bytes, Sledgehammer achieves this in 0.75 to 2.5 bytes. Optimized Sledgehammer code should be <1/2 the size of Mthmtca code.

### Setup

Instructions for Windows. After cloning this directory, run the setup script with

    wolframscript -script setup.wls

then run a program like

    wolframscript -script hammer.wls v examples/hello.hmr

or 

    wolframscript -script hammer.wls vd examples/fizzbuzz.hmr

To take input from a file, use

    wolframscript -script hammer.wls v in.txt examples/primeq.hmr examples/primenumber.txt

Arguments in an input file should be separated by newlines.

Flags:

    v    Run verbose code from file, and check compression/decompression.
    w    Run Wolfram-style code from file, and check compression/decompression.
    s    Suppress output of top of stack.

Unimplemented flags:

    f    Run compressed code from file. Default for code-golf scoring purposes.
    c    Compress file and write to .hmrc file.
    C    Decompress compressed source and write to a .hmr file.
    b    Compress into Braille.
    B    Decompress from Braille.
    d    Print debug information. If a builtin was designed for a particular PPCG challenge, the question number will be printed.
    h    Print the history.
    x    Print explanation of code from Mathematica or Sledgehammer docs.


### Syntax for verbose source code

Tokens are separated by spaces. For functions with more than 3 arguments, concatenate them with Sequence, List, and Apply.

    42                    The integer literal 42. Stored using modified Elias Delta coding.
    "forty-two"           The string literal "forty-two".
    \StringReplace        The Wolfram symbol StringReplace.
    StringReplace.0       Wolfram builtin StringReplace[] called with 0 arguments.
    StringReplace         Wolfram bulitin StringReplace called with 1 argument.
    StringReplace.2       Wolfram builtin StringReplace called with 2 arguments.
    StringReplace.3       Wolfram builtin StringReplace called with 3 arguments.
    StringReplace.312     Switch argument order. x1 x2 x3 StringReplace.312 is equivalent to StringReplace[x3,x1,x2].
                            Not defined for commutative functions.
