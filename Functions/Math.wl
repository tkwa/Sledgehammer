(* ::Package:: *)

BeginPackage["Sledgehammer`"];

Begin["`Private`"];


(* ::Subsection:: *)
(*Arithmetic*)


SubtractOne = # - 1&;
PlusOne = # + 1&;

plusMinus[x: _List | _Association] := Join[x, -x]&
plusMinus[x_] := {x, -x};


Double = 2# &;
Halve = #/2 &;


SymmetricMod2n[m_, n_] := Mod[m, 2n, -n+1];
SymmetricMod[m_, n_] := Mod[m, n, -n/2+.5];



(* ::Subsection:: *)
(*Primes*)


FactorizationExponents[n_] := Last /@ FactorInteger@n;
SemiprimeQ[n_] := 2 == Total[Last /@ FactorInteger@n];

DistinctPrimeFactors[n_] := First /@ FactorInteger@n;
SquareFreePart[n_] := Times @@ (First /@ FactorInteger@n);

(* To do: Generalize to rational n and m *)
SetAttributes[FactorMultiplicity, Listable]
FactorMultiplicity[n_, m_] := Module[{x=0, nn = n},
	While[Divisible[nn, m], x++; nn /= m] ; x
];


End[];
EndPackage[];
