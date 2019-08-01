(* ::Package:: *)

BeginPackage["Sledgehammer`"];



(* ::Section:: *)
(*Package Symbols*)


Sledgehammer`Private`$PackageDirectory::usage = "The directory where Sledgehammer is installed.";
Sledgehammer`Private`$spf::usage = "The SequencePredictorFunction used in the main compression algorithm.";
Sledgehammer`Private`$tokenModel::usage = "Token model based on $spf";



(* ::Section:: *)
(*Utilities*)


(* ::Subsection:: *)
(*Arrays*)


IndexRange::usage = "APL's monadic iota.";
Indices::usage = "APL's monadic iota-rho.";
FlatIndices::usage = "Multidimensional indices as a list.";
ArrayBand::usage = "ArrayBand[mat, {2, 1}, All, {1,1}] gives the diagonal above the main diagonal.";
Bands::usage = "Bands of an array.";
LongestBands::usage = "All longest bands of an array for some fixed step";


(* ::Subsection:: *)
(*Automata*)


(* See Utilities/Automata.wl for documentation *)
Turmite2DBounded;
Turmite2DBoundedResult;
Mouse2DBounded;
Mouse2DBoundedResult;


(* ::Subsection:: *)
(*Constants*)


(* ::Subsection:: *)
(*Entities*)


EntityNthProperty::usage = "Gets the nth property of entities.";


(* ::Subsection:: *)
(*ExternalLangs*)


JellyCodepage::usage = "The Jelly codepage.";


(* ::Subsection:: *)
(*Functional*)


left::usage = "Returns the first argument.";
right::usage = "Returns the last argument.";
Fork1::usage = "An expression wrapped in fork1 will have all symbols applied to #, except inside Compose or tacitVerbatim.";
Fork2::usage = "Applies all symbols to {#,#2}.";
Tacit2::usage = "Uses integer last argument to determine whether each symbol is applied to #, {#, #2}, or #2.";

Under::usage = "Under[f, g] is InverseFunction[g] @* f @* g.";

SelectInteger::usage = "SelectInteger[f_, k_Integer: 1] is the least integer >= k where f returns True.";
SelectIntegers::usage "SelectIntegers[f_, n_Integer, k_Integer: 1] is a list of the first n integers >= k where f returns True.";



(* ::Subsection:: *)
(*General*)


GetSet::usage = "Sets a variable and returns its old value.";

SetRegister::usage = "Stores a value in the global register.";
GetRegister::usage = "Gets a value from the global register.";


(* ::Subsection:: *)
(*Lists*)


RLEncode::usage = "Run-length encode. {2,2,2,7,1,1,1,1,8} -> {{2,3},{7,1},{1,4},{8,1}}.";
RLDecode::usage = "Run-length decode; inverts rleEncode.";

(* Vectorization *)
VEqual::usage = "Vectorized equal.";
BooleVEqual::usage = "Converts vectorized equal to 0/1.";
MapTotal::usage = "Maps Total onto each element of a list";
ListProduct::usage = "Product of a list (Fold[Times]).";

Replicate::uage = "APL's Replicate (/).";
ArrayExpand::usage = "APL's Expand (\).";

ListHalve::usage = "Splits l/a/n/s into two equal parts; with the first list getting the odd element.";
MaxByLength;
MinByLength;
SubsetSums::usage = "All unique sums of subsets of l, in sorted order.";

SwapProperties::usage = "Swaps properties of two lists. SwapProperties[getCase_, setCase_, canonicalize_: Identity].";


(* ::Subsection:: *)
(*Math*)


SubtractOne;
PlusOne;
plusMinus::usage = "Returns the list {x, -x}; on a list l returns Flatten[plusMinus/@l].";
Double;
Halve;
SymmetricMod2n::usage = "Mod 2n with offset. symmetricMod2n[{14, 15, 16},5] = {4, 5, -4}.";
SymmetricMod::usage = "symmetricmodn[m, 2n] = symmetricmod2n[m, n].";

FactorizationExponents::usage = "Exponenents of prime factorization.";
SemiprimeQ::usage = "Whether a number is the product of two (possibly equal) primes.";
DistinctPrimeFactors::usage = "";
SquareFreePart::usage = "The product of a number's distinct prime factors.";
FactorMultiplicity::usage = "Number of times y divides x";


(* ::Subsection:: *)
(*Sets*)


FromCounts::usage = "Inverts Counts. fromCounts@*Counts sorts by first order of appearance.";
BagIntersection::usage = "Multiset intersection.";
BagUnion::usage = "Multiset union.";


(* ::Section:: *)
(*Arithcoder*)


encode;
decode;

SHEncode::usage = "Arithmetic encodes a sequence of tokens."
SHDecode::usage = "Arithmetic decodes a sequence of tokens."


(* ::Section:: *)
(*Interpreter*)


(* Literals *)
varEliasDelta;
unVarEliasDelta;

(* Interpreter *)
preprocess;
postprocess;
wToPostfix;
postfixToW;
compress::usage = "Compresses a postfix expression using the arithmetic coder.";
decompress;
tokenToBits;
bitsToToken;
eval::usage = "eval[expr, args] evaluates a HoldComplete expression expr given a list of arguments args.";
SledgehammerGUI;


EndPackage[]
