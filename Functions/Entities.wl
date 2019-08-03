(* ::Package:: *)

BeginPackage["Sledgehammer`"];

Begin["Private`"];

EntityNthProperty[name_String,type_String,N_Integer] := 
	EntityValue[Entity[type,name],EntityProperties[type][[N]]];
	


End[];
EndPackage[];
