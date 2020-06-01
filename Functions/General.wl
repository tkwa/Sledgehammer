(* ::Package:: *)

BeginPackage["Sledgehammer`"];

Begin["`Private`"];

(* Variable manipulation *)


GetSet[sym_Symbol, val_] := With[{tmp = sym}, Set[sym, val]; tmp]
SetAttributes[GetSet, HoldFirst]

SetRegister[val_] := ($SHRegister = val);
GetRegister[val_] := $SHRegister;

End[];
EndPackage[];
