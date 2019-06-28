(* ::Package:: *)

(* ::Text:: *)
(*All functions for export are defined using DownValues so their arguments can be extracted using the setup file.*)
(*(How to deal with cases like Left and Right?)*)
(**)
(*Functions that take multiple arguments are written so as to modify their first argument.*)


(* ::Subsection:: *)
(*Definitions and usage...*)


BeginPackage["SHUtils`"];
(* Math *)
minusOne;
plusOne;
plusMinus::usage = "Returns the list {x, -x}; on a list l returns Flatten[plusMinus/@l].";
vTotal::usage = "Totals each element of a list.";
double;
halve;
product;
symmetricMod2n::usage = "Mod 2n with offset. symmetricMod2n[{14, 15, 16},5] = {4, 5, -4}.";
symmetricmodn::usage = "symmetricmodn[m, 2n] = symmetricmod2n[m, n].";

factorizationExponents::usage = "Exponenents of prime factorization.";
semiprimeQ::usage = "Whether a number is the product of two (possibly equal) primes.";
distinctPrimeFactors;
makeSquarefree::usage = "The product of a number's distinct prime factors.";
factorMultiplicity::usage = "Number of times y divides x";

(* List manipulation *)
rleEncode::usage = "Run-length encode. {2,2,2,7,1,1,1,1,8} -> {{2,3},{7,1},{1,4},{8,1}}.";
rleDecode::usage = "Run-length decode; inverts rleEncode.";
vEqual::usage = "Vectorized equal.";
boolevEqual::usage = "Converts vectorized equal to 0/1.";
replicate::uage = "APL's Replicate (/).";
expand::usage = "APL's Expand (\).";

listHalve::usage = "Splits l/a/n/s into two equal parts; with the first list getting the odd element.";
longest;
shortest;
subsetSums::usage = "All unique sums of subsets of l, in sorted order.";

fromCounts::usage = "Inverts Counts; fromCounts@*Counts sorts by first order of appearance.";
bagIntersection::usage = "Multiset intersection.";
bagUnion::usage = "Multiset union.";

(* Entities *)
entityNthProperty::usage = "Gets the nth property of entities.";

(* Functional *)
left::usage = "Returns the first argument.";
right::usage = "Returns the last argument.";
fork1::usage = "An expression wrapped in fork1 will have all symbols applied to #, except inside Compose or tacitVerbatim.";
fork2::usage = "Applies all symbols to {#,#2}.";
tacit2::usage = "Uses last argument to determine whether each symbol is applied to #, {#, #2}, or #2.";

nFind::usage = "f -> n_:1 -> First n positive integers where f returns True.";
nFindFrom::usage = "f -> k -> First positive integer >= k where f returns True.";

(* Constants... *)

Begin["Private`"];
(* maybe unprotect everything? *)
unprotect[f_Symbol] := If[! MemberQ[Attributes@f,Locked], Unprotect[f]];
(* Associate definitions with InverseFunction so we don't need to protect everything. 
Must load InverseFunction first so the Unprotect takes...*)
InverseFunction;
mutualInverses[f_Symbol, g_Symbol] := (Unprotect@InverseFunction; InverseFunction[f] = g; InverseFunction[g] = f;Protect@InverseFunction)


(* ::Section::Closed:: *)
(*Math*)


(* ::Subsection::Closed:: *)
(*Arithmetic*)


(* ::Subsubsection::Closed:: *)
(*Overloads*)


Unprotect[Plus, Times, Max, Min];
(* 0 + True still gives True... *)
Plus@True := 1
Plus@False := 0
Times@True := 1
Times@False := 0
Max@True := 1
Max@False := 0
Min@True := 1
Min@False := 0


(* ::Subsubsection::Closed:: *)
(*General arithmetic*)


minusOne = # - 1&;
plusOne = # + 1&;

plusMinus[x: _List | _Association] := Join[x, -x]&
plusMinus[x_] := {x, -x};


vTotal = Map@Total;

double = 2# &;
halve = #/2 &;

product[l_List] := Fold[Times, l];
product[x_] := x;

symmetricMod2n[m_, n_] := Mod[m, 2n, -n+1];
symmetricModn[m_, n_] := Mod[m, n, -n/2+.5];



(* ::Subsection::Closed:: *)
(*Primes*)


factorizationExponents[n_] := Last /@ FactorInteger@n;
semiprimeQ[n_] := 2 == Total[Last /@ FactorInteger@n];

distinctPrimeFactors[n_] := First /@ FactorInteger@n;
makeSquarefree[n_] := Times @@ (First /@ FactorInteger@n);

(* To do: Generalize to rational n and m *)
SetAttributes[factorMultiplicity, Listable]
factorMultiplicity[n_, m_] := Module[{x=0, nn = n},
	While[Divisible[nn, m], x++; nn /= m] ; x
];


(* ::Section:: *)
(*List manipulation*)


(* ::Subsection:: *)
(*General list processing*)


(* ::Subsubsection::Closed:: *)
(*Overloads*)


Unprotect[Select, Map];
Select[n_Integer, f_, k_.] := Select[Range@n, (Head@# == List && Length@# > 0 || NumericQ@# && # != 0 || TrueQ@#&)@*f, k]
Map[expr_, n_Integer] := Map[fork1[expr], Range@n] (* since known to have depth 1 *)


(* ::Subsubsection:: *)
(*General list processing*)


rleEncode[l___] := Split[l] // Map[{First@#, Length@#}&];
rleDecode[{runs___}] := Catenate[Table @@@ {runs}];
mutualInverses[rleEncode, rleDecode];

SetAttributes[vEqual, Listable]
vEqual[x_?AtomQ, y_?AtomQ] := x == y
BoolevEqual = Boole @* vEqual;


(* ::Subsection::Closed:: *)
(*APL-like functions*)


(* From APL, works only on rank 1 numeric arrays for now *)
replicate[l_, ns_] := MapThread[ConstantArray, {l, ns}] // Catenate

(* From APL, placeholder *)
expand[l_, ns_] := Module[{zip = Thread[{l, ns}]},
	Null
];



(* ::Subsection::Closed:: *)
(*Partitioning*)


Unprotect@Partition;
Partition[n_Integer, args___] := Partition[Range@n, args]

listHalve[l: _List | _Association] := Partition[l, Length[l]/2]
listHalve[n_Integer] := Partition[Range@n, n/2]
listHalve[s_String] := StringPartition[s, StringLength[s]/2]


(* ::Subsubsection:: *)
(*Selecting a single element by criteria*)


longest = MaximalBy[Length];
shortest = MinimalBy[Length];


(* ::Subsubsection:: *)
(*Subsets and permutations*)


subsetSums[l_] := Union[Total /@ Subsets@l];


(* ::Subsection::Closed:: *)
(*Sets and multisets*)


(* ::Subsubsection:: *)
(*Multisets*)


(* To add: symdiff, diffCounts, complement (ordered by first), equal.*)

(* Inverts Counts. fromCounts@*Counts sorts by first order of appearance. *)
fromCounts[a_] := Normal[a] // ConstantArray@@@#& // Catenate;
mutualInverses[Counts, fromCounts]

bagIntersection[l1_, l2_] := Merge[{Counts[l1], Counts[l2]}, Min] // fromCounts;
bagUnion[l1_, l2_] := Merge[{Counts[l1], Counts[l2]}, Max] // fromCounts;


(* ::Subsection::Closed:: *)
(*Replacement*)


(* ::Subsubsection:: *)
(*Overloads*)


Unprotect@ReplaceAll;
ReplaceAll[expr_, r_TwoWayRule] := ReplaceAll[expr, {Rule@@r, Rule@@Reverse@r}];
ReplaceAll[expr_, r:{Repeated@_TwoWayRule}] := ReplaceAll[expr, Riffle[Rule@@@r, Rule@@@Reverse/@r]];

Unprotect@StringReplace;
StringReplace[expr_, r_TwoWayRule] := StringReplace[expr, {Rule@@r, Rule@@Reverse@r}];
StringReplace[expr_, r:{Repeated[_TwoWayRule]}] := StringReplace[expr, Riffle[Rule@@@r, Rule@@@Reverse/@r]];



(* Generalize swapping cases of two strings 
f[x]: get the "case" of an element
g[x, c]: set the "case" of an element
h[x] := put x in "lowercase" (canonical form)
*)
swapProperties[f_, g_, h_, l1_, l2_] := Module[{},
	{MapThread[g, {h /@ l1, f /@ l2}],
	MapThread[g, {h /@ l2, f /@ l1}]}
];
swapProperties[f_, g_, l1_, l2_] := swapProperties[f, g, Identity, l1, l2];



(* ::Section::Closed:: *)
(*Entity builtins*)


entityNthProperty[name_String,type_String,N_Integer] := 
	EntityValue[Entity[type,name],EntityProperties[type][[N]]];


(* ::Section:: *)
(*Functional builtins*)


(* ::Subsubsection::Closed:: *)
(*Identities*)


left[a_, args___] := a;
right[args___, w_] := w;


(* ::Subsection::Closed:: *)
(*Tacit functions*)


(*
Does not enter heads tacitVerbatim and Compose.
Source: https://mathematica.stackexchange.com/a/201075/61597
*)
SetAttributes[{fork1, hide}, HoldFirst]
fork1[e_]:=With[{g = hide[e]},
    Replace[
        g[[1]],
        s_Symbol /; !MemberQ[Attributes[s], Temporary] :> s[#],
        {2, Infinity}
    ] /. g[[2]]
]
hide[e_] := Reap[
    ReplaceAll[
        Function[e],
        {SHUtils`tacitVerbatim [t_] :> RuleCondition @ Module[{x}, Sow[x, Unevaluated[t]]],
         c_Compose :> RuleCondition@Module[{x}, Sow[x, Unevaluated@c]]
         }
    ],
    _,
    toRuleDelayed
]
SetAttributes[toRuleDelayed,HoldAll]

toRuleDelayed[x_, {s_}] := s :> x

fork1[expr_, arg_] := fork1[expr]@arg;


(* ::Subsubsection:: *)
(*Inverses and conjugation*)


Unprotect@InverseFunction;


InverseFunction@Reverse := Reverse
mutualInverses[IntegerDigits,FromDigits]


under[f_, g_] := InverseFunction[g]@*f@*g

(* Inverts the most recent function with a defined inverse. *)
autoUnder=.


(* under[Reverse, IntegerDigits]@57 = 75 *)


(* ::Subsection::Closed:: *)
(*Sequences*)


(* Smallest positive integer where property f is True (todo: or nonzero) 
todo: add options for *)
nFind[f_] := NestWhile[#+1&, 1, Not@*f]
nFindFrom[f_, k_Integer] := NestWhile[#+1&, k+1, Not@*f]

(* Smallest n positive integers where property f is True *)
nFind[f_, n_Integer] := NestList[nFindFrom[f, #]&, 1, n-1]


(* ::Section::Closed:: *)
(*Constants*)


(* ::Subsubsection:: *)
(*Integer lists*)


c00 = {0, 0};
c01 = {0, 1};
c10 = {1, 0};
c11 = {1, 1};
c12 = {1, 2};
c22 = {2, 2};
c23 = {2, 3};
c33 = {3, 3};
c1m1 = {1, -1};
cm11 = {-1, 1};

c222 = {2, 2, 2};
c235 = {2, 3, 5};
cm101 = {-1, 0, 1};


(* ::Subsubsection:: *)
(*Strings*)


(* ::Section:: *)
(*Builtins from other sources*)


(* ::Subsection:: *)
(*Mathematica.SE and PPCG.SE answers*)


(* define code extractor here *)

(* site goes from 0-3, ID = 4*answer ID + site 
0: PPCG
1: Mma.SE
2: OEIS?
*)
corpusFunction[ID_Integer,idx_Integer] = Module[{},
Null
];
corpusFunction[ID_Integer] := nthSEFn[ID, 1]



(* ::Subsection:: *)
(*Builtins from other code-golf languages*)


(* ::Subsubsection:: *)
(*Jelly*)


jellyCodepage = "\[DownExclamation]\[Cent]\[Sterling]\[Currency]\[Yen]\.a6\[Copyright]\[Not]\[RegisteredTrademark]\[Micro]\.bd\[DownQuestion]\[Euro]\[CapitalAE]\[CapitalCCedilla]\[CapitalEth]\[CapitalNTilde]\[Times]\[CapitalOSlash]\[CapitalOE]\[CapitalThorn]\[SZ]\[AE]\[CCedilla]\[Eth]\[DotlessI]\:0237\[NTilde]\[Divide]\[OSlash]\[OE]\[Thorn] !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\[Paragraph]\[Degree]\.b9\.b2\.b3\:2074\:2075\:2076\:2077\:2078\:2079\:207a\:207b\:207c\:207d\:207e\:0181\:0187\:018a\:0191\:0193\:0198\:2c6e\:019d\:01a4\:01ac\:01b2\:0224\:0253\:0188\:0257\[Florin]\:0260\:0266\:0199\:0271\:0272\:01a5\:02a0\:027c\:0282\:01ad\:028b\:0225\:1ea0\:1e04\:1e0c\:1eb8\:1e24\:1eca\:1e32\:1e36\:1e42\:1e46\:1ecc\:1e5a\:1e62\:1e6c\:1ee4\:1e7e\:1e88\:1ef4\:1e92\:0226\:1e02\:010a\:1e0a\:0116\:1e1e\:0120\:1e22\:0130\:013f\:1e40\:1e44\:022e\:1e56\:1e58\:1e60\:1e6a\:1e86\:1e8a\:1e8e\:017b\:1ea1\:1e05\:1e0d\:1eb9\:1e25\:1ecb\:1e33\:1e37\:1e43\:1e47\:1ecd\:1e5b\:1e63\:1e6d\[Section]\[CapitalADoubleDot]\:1e89\:1ef5\:1e93\:0227\:1e03\:010b\:1e0b\:0117\:1e1f\:0121\:1e23\:0140\:1e41\:1e45\:022f\:1e57\:1e59\:1e61\:1e6b\:1e87\:1e8b\:1e8f\:017c\[LeftGuillemet]\[RightGuillemet]\[OpenCurlyQuote]\[CloseCurlyQuote]\[OpenCurlyDoubleQuote]\[CloseCurlyDoubleQuote]";


End[];

EndPackage[]
