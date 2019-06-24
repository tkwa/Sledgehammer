(* ::Package:: *)

BeginPackage["SHUtils`"];


(* ::Section:: *)
(*Math*)


(* ::Subsection:: *)
(*Arithmetic*)


minusOne = # - 1&;
plusOne = # + 1&;

plusMinus[x: _List | _Association] := Join[x, -x]&
plusMinus[x_] := {x, -x};

sumEach = Map@Total;

double = 2# &;
halve = #/2 &;

product[l_List] := Fold[Times, l];
product[x_] := x;

symmetricMod2n[m_, n_] := Mod[m, 2n, -n+1];
symmetricModn[m_, n_] := Mod[m, n, -n/2+.5];


(* ::Subsection:: *)
(*Primes*)


factorizationExponents[n_] := Last /@ FactorInteger@n;
semiprimeQ[n_] := 2 == Total[Last /@ FactorInteger@n];

distinctPrimeFactors[n_] := First /@ FactorInteger@n;
makeSquarefree[n_] := Times @@ (First /@ FactorInteger@n);


(* ::Section:: *)
(*List manipulation*)


rleEncode[l___] := Split[l] // Map[{First@#, Length@#}&];
rleDecode[runs___] := Catenate[Table @@@ runs];

SetAttributes[vEqual, Listable]
vEqual[x_?AtomQ, y_?AtomQ] := x == y
vEqualBin = Boole @* vEqual;


(* ::Subsubsection:: *)
(*Partitioning*)


Unprotect@Partition;
Partition[n_Integer, args___] := Partition[Range@n, args]

listHalve[l_] := Partition[l, Length[l]/2]
listHalve[n_Integer] := Partition[n, n/2]
listHalve[s_String] := StringPartition[s, StringLength[s]/2]


(* ::Subsubsection:: *)
(*Selecting*)


longest = MaximalBy[Length];
shortest = MinimalBy[Length];


(* ::Subsubsection:: *)
(*Subsets and permutations*)


subsetSums[l_] := DeleteDuplicates[Total /@ Subsets@l];


(* ::Section:: *)
(*Replacement*)


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



(* ::Section:: *)
(*Entity builtins*)


entityNthProperty[name_String,type_String,N_Integer] := 
	EntityValue[Entity[type,name],EntityProperties[type][[N]]];


(* ::Section:: *)
(*Functional builtins*)


(* ::Subsubsection:: *)
(*Identities*)


left[a_, args___] := a;
right[args___, w_] := w;


(* ::Subsubsection:: *)
(*Tacit functions*)


SetAttributes[fork$1, HoldFirst]
fork$1[expr_] := Function[expr /. s_Symbol -> s[#]];


1+1 /. 1 -> 5


(* ::Subsubsection:: *)
(*Inverses and conjugation*)


under[f_Function, g_Function] := InverseFunction[g]@f@g@#&
under[f_Function, g_] := under[f, Function@g]
under[f_, g_] := under[Function@f, g]


unprotect[f_Symbol] := If[MemberQ[Attributes@f,Locked], Unprotect[f],Null];



(* ::Subsubsection:: *)
(*Sequences*)


(* Smallest positive integer where property f is True *)
nfind[f_] := NestWhile[#+1&, 1, !f]


(* ::Section:: *)
(*Nilads*)


(* ::Subsubsection:: *)
(*Integer lists*)


n00 = {0, 0};
n01 = {0, 1};
n10 = {1, 0};
n11 = {1, 1};
n12 = {1, 2};
n22 = {2, 2};
n23 = {2, 3};
n33 = {3, 3};
n1m1 = {1, -1};
nm11 = {-1, 1};

n222 = {2, 2, 2};
n235 = {2, 3, 5};
nm101 = {-1, 0, 1};


(* ::Subsubsection:: *)
(*Strings*)


Str


(* ::Section:: *)
(*Builtins from other sources*)


(* ::Subsection:: *)
(*Mathematica.SE and PPCG.SE answers*)


Begin["`Private`"]
(* define code extractor here *)
End[]

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


EndPackage[]
