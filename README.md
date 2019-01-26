# Sledgehammer

Sledgehammer is a stack-based code-golf language written in, and based on, Wolfram Language. Its goal is to minimize size of compressed source. Sledgehammer consists of two parts:

* A compressor/decompressor for Wolfram Language expressions
* A code-golf library (not yet implemented)

### Prior art

While [Mthmtca](https://github.com/LegionMammal978/Mthmtca) compresses each Mathematica builtin into 4 bytes, Sledgehammer achieves this in 0.75 to 2.5 bytes. Optimized Sledgehammer code should be about 1/2 the size of Mthmtca code, and comparable to code size in other golfing languages.

### Setup

Instructions for Windows. After cloning this directory, run the setup script with

    wolframscript -script setup.wls

then run a program like

    wolframscript -script hammer.wls w examples/hello.hmr

To take input from a file, use

    wolframscript -script hammer.wls b examples/primeq.hmrb examples/primenumber.txt

Arguments in an input file should be separated by newlines.

Flags:

    w    Run Wolfram Language-style code from file, and check compression/decompression.
    c    Run compressed code from file. Default for code-golf scoring purposes.
    b    Run UTF-8 encoded Braille source code from file.
    d    Print debug information to stdout and compressed/uncompressed form to files.
    n    Do not execute the code (but still check compression).

Unimplemented flags:

    l    Print information about Sledgehammer builtins.
    x    Print explanation of code from Mathematica or Sledgehammer docs.