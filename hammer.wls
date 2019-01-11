#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Subsection:: *)
(*Imports and setup*)


parentPath = $InputFileName /. "" :> NotebookFileName[];
parentDir = DirectoryName @ parentPath;
SetDirectory[parentDir];
Get["save.mx", Path -> Directory[]];
With[{printShows = False}, Get["interpreter.wls", Path -> Directory[]]]
On@Assert

flags  = $ScriptCommandLine[[2]] // Characters;
(* check flags *)

validFlags = Characters@"fvcCbBhdxs"
badFlags = Complement[flags, validFlags]

Assert[Length@badFlags == 0, "Invalid flags" <> StringJoin@badFlags];

If[ContainsAny[flags, {"b","B"}], Print["Braille not implemented"]];
printDebug = MemberQ[flags, "d"];
suppress = MemberQ[flags, "s"];
If[MemberQ[flags, "h"], Print["Print history not implemented"]];
If[MemberQ[flags, "x"], Print["Explanation not implemented"]];

isVerbose = ContainsAny[flags, Characters@"vc"];
If[isVerbose && ContainsAny[flags, Characters@"fC"],
	Print["Cannot (de)compress already (de)compressed code!"]]



(* ::Subsection:: *)
(*Execution*)


isExecute = ContainsAny[flags, Characters@"fv"];

codeFile = $ScriptCommandLine[[3]];

Assert[isVerbose];

code = If[isVerbose,
	splitTokens@ReadString@codeFile];

(* compression should be reversible *)
Assert[decompress@compress@code == code];
debugHistory = {};
If[printDebug,
	Export["_tokenized.mx", {Length@tokenToBits@#, #} &/@code, "Table"];
	Export["_compressed.mx", compressed = compress@code, "String"];
	Print["Compressed code exported to debug files"];
	codeLength = Length@compress@code;
	Print["Code length: ", codeLength, " bits = ", N@codeLength/8, " bytes"];
];

input = If[Length@$ScriptCommandLine >= 4,
	ReadList@$ScriptCommandLine[[4]],
	{}
];

Print@eval[code, input]

If[printDebug,
	Export["_debugHistory.mx", debugHistory, "List"]]



