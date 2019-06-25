# Sledgehammer

Sledgehammer is a stack-based code-golf language written in, and based on, Wolfram Language. Its goal is to minimize size of compressed source. Sledgehammer consists of two parts:

* A compressor/decompressor for Wolfram Language expressions
* A code-golf library (not yet implemented as of 0.4)

### Prior art

While [Mthmtca](https://github.com/LegionMammal978/Mthmtca) compresses each Mathematica token into 4 bytes, Sledgehammer achieves an average compression ratio of 1 to 1.3 bytes per token. Optimized Sledgehammer code size should be comparable to that of other golfing languages.

### Instructions for use

For all non-reference purposes, use the interactive app.

To use Sledgehammer from the command line (on Windows), run a program like

    wolframscript -script hammer.wls w codeFile.hmr

To take input from a file, use

    wolframscript -script hammer.wls b codeFile.txt inputFile.txt

Arguments in an input file should be separated by newlines.

Flags:

    w    Run Wolfram Language code from file, and check compression/decompression.
    c    Run compressed code from file. Default for code-golf scoring purposes.
    b    Run UTF-8 encoded Braille source code from file.
    d    Print debug information to stdout and compressed/uncompressed form to files.
    n    Do not execute the code (but still check compression).

Unimplemented flags:

    l    Print information about Sledgehammer builtins.
    s	   Format the code as a Stack Exchange Code Golf submission.
    x    Print explanation of code from Mathematica or Sledgehammer docs.
