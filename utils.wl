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

unprotect[f_Symbol] := If[MemberQ[Attributes@f,Locked], Unprotect[f],Null];

