(* ::Package:: *)

BeginPackage["Sledgehammer`"];

Begin["`Private`"];

FromCounts[a_] := Normal[a] // ConstantArray@@@#& // Catenate;

BagIntersection[l1_, l2_] := Merge[{Counts[l1], Counts[l2]}, Min] // FromCounts;

BagUnion[l1_, l2_] := Merge[{Counts[l1], Counts[l2]}, Max] // FromCounts;

End[]
EndPackage[]
