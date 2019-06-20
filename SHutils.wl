(* ::Package:: *)

BeginPackage["SHUtils`"]


(* ::Subsubsection:: *)
(*Entity builtins*)


entityNthProperty[name_String,type_String,N_Integer] := 
	EntityValue[Entity[type,name],EntityProperties[type][[N]]];


(* ::Subsubsection:: *)
(*Functional builtins*)


under[f_Function, g_Function] := InverseFunction[g]@f@g@#&
under[f_Function, g_] := under[f, Function@g]
under[f_, g_] := under[Function@f, g]


unprotect[f_Symbol] := If[MemberQ[Attributes@f,Locked], Unprotect[f],Null];

EndPackage[]





(* ::Subsubsection:: *)
(*Builtins from other code-golf languages*)


jellyCodepage = "\[DownExclamation]\[Cent]\[Sterling]\[Currency]\[Yen]\.a6\[Copyright]\[Not]\[RegisteredTrademark]\[Micro]\.bd\[DownQuestion]\[Euro]\[CapitalAE]\[CapitalCCedilla]\[CapitalEth]\[CapitalNTilde]\[Times]\[CapitalOSlash]\[CapitalOE]\[CapitalThorn]\[SZ]\[AE]\[CCedilla]\[Eth]\[DotlessI]\:0237\[NTilde]\[Divide]\[OSlash]\[OE]\[Thorn] !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\[Paragraph]\[Degree]\.b9\.b2\.b3\:2074\:2075\:2076\:2077\:2078\:2079\:207a\:207b\:207c\:207d\:207e\:0181\:0187\:018a\:0191\:0193\:0198\:2c6e\:019d\:01a4\:01ac\:01b2\:0224\:0253\:0188\:0257\[Florin]\:0260\:0266\:0199\:0271\:0272\:01a5\:02a0\:027c\:0282\:01ad\:028b\:0225\:1ea0\:1e04\:1e0c\:1eb8\:1e24\:1eca\:1e32\:1e36\:1e42\:1e46\:1ecc\:1e5a\:1e62\:1e6c\:1ee4\:1e7e\:1e88\:1ef4\:1e92\:0226\:1e02\:010a\:1e0a\:0116\:1e1e\:0120\:1e22\:0130\:013f\:1e40\:1e44\:022e\:1e56\:1e58\:1e60\:1e6a\:1e86\:1e8a\:1e8e\:017b\:1ea1\:1e05\:1e0d\:1eb9\:1e25\:1ecb\:1e33\:1e37\:1e43\:1e47\:1ecd\:1e5b\:1e63\:1e6d\[Section]\[CapitalADoubleDot]\:1e89\:1ef5\:1e93\:0227\:1e03\:010b\:1e0b\:0117\:1e1f\:0121\:1e23\:0140\:1e41\:1e45\:022f\:1e57\:1e59\:1e61\:1e6b\:1e87\:1e8b\:1e8f\:017c\[LeftGuillemet]\[RightGuillemet]\[OpenCurlyQuote]\[CloseCurlyQuote]\[OpenCurlyDoubleQuote]\[CloseCurlyDoubleQuote]"
