(* ::Package:: *)

BeginPackage["Sledgehammer`"];

Begin["`Private`"];


(* ::Subsection:: *)
(*General list processing*)


RLEncode[l___] := Split[l] // Map[{First@#, Length@#}&];
RLDecode[{runs___}] := Catenate[Table @@@ {runs}];

(* APL's / , works only on rank 1 numeric arrays for now *)
Replicate[l_, ns_] := MapThread[ConstantArray, {l, ns}] // Catenate

(* APL's \ . Placeholder. *)
ArrayExpand[l_, ns_] := Module[{zip = Thread[{l, ns}]},
	Null
];

ListHalve[l: _List | _Association] := Partition[l, Length[l]/2]
ListHalve[n_Integer] := Partition[Range@n, n/2]
ListHalve[s_String] := StringPartition[s, StringLength[s]/2]


(* ::Subsection:: *)
(*Vectorization*)


SetAttributes[VEqual, Listable]
VEqual[x_?AtomQ, y_?AtomQ] := x == y
BooleVEqual = Boole @* VEqual;

MapTotal = Map@Total;

ListProduct[l_List] := Fold[Times, l];
product[x_] := x;


(* ::Subsection:: *)
(*Filtering*)


MaxByLength = MaximalBy[Length];
MinByLength = MinimalBy[Length];


(* ::Subsection:: *)
(*Subsets and permutations*)


SubsetSums[l_] := Union[Total /@ Subsets@l];


(* ::Subsection:: *)
(*Replacement*)


(* Generalize swapping cases of two strings 
f[x]: get the "case" of an element
g[x, c]: set the "case" of an element
h[x] := put x in "lowercase" (canonical form)
*)
SwapProperties[f_, g_, h_, l1_, l2_] := Module[{},
	{MapThread[g, {h /@ l1, f /@ l2}],
	MapThread[g, {h /@ l2, f /@ l1}]}
];
SwapProperties[f_, g_, l1_, l2_] := swapProperties[f, g, Identity, l1, l2];




(* ::Subsection:: *)
(**)


End[];
EndPackage[];
