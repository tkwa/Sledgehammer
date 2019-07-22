(* ::Package:: *)

BeginPackage["Sledgehammer`"];

Begin["`Private`"];
(* maybe unprotect everything? *)
unprotect[f_Symbol] := If[! MemberQ[Attributes@f,Locked], Unprotect[f]];
(* Associate definitions with InverseFunction so we don't need to protect everything. 
Must load InverseFunction first so the Unprotect takes...*)
InverseFunction;
mutualInverses[f_Symbol, g_Symbol] := (Unprotect@InverseFunction; InverseFunction[f] = g; InverseFunction[g] = f;Protect@InverseFunction)

Unprotect[Select, Map];
Select[n_Integer, f_, k_.] := Select[Range@n, (Head@# == List && Length@# > 0 || NumericQ@# && # != 0 || TrueQ@#&)@*f, k]
Map[expr_, n_Integer] := Map[fork1[expr], Range@n] (* since known to have depth 1 *)

Unprotect@ReplaceAll;
ReplaceAll[expr_, r_TwoWayRule] := ReplaceAll[expr, {Rule@@r, Rule@@Reverse@r}];
ReplaceAll[expr_, r:{Repeated@_TwoWayRule}] := ReplaceAll[expr, Riffle[Rule@@@r, Rule@@@Reverse/@r]];

Unprotect@StringReplace;
StringReplace[expr_, r_TwoWayRule] := StringReplace[expr, {Rule@@r, Rule@@Reverse@r}];
StringReplace[expr_, r:{Repeated[_TwoWayRule]}] := StringReplace[expr, Riffle[Rule@@@r, Rule@@@Reverse/@r]];

End[];
EndPackage[];
