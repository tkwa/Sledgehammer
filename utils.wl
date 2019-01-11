(* ::Package:: *)

entityNthProperty[name_String,type_String,N_Integer] := 
	EntityValue[Entity[type,name],EntityProperties[type][[N]]];

Assert[entityNthProperty["Canada","Country",16] == \!\(\*
TagBox[
StyleBox[
RowBox[{"Quantity", "[", 
RowBox[{"6.89021071071566`", ",", "\"\<Percent\>\""}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)];

under[f_Function, g_Function] := InverseFunction[g]@f@g@#&
under[f_Function, g_] := under[f, Function@g]
under[f_, g_] := under[Function@f, g]


unprotect[f_Symbol] := If[MemberQ[Attributes@f,Locked], Unprotect[f],Null];

