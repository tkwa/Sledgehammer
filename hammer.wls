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

validFlags = Characters@"fvcCbBhdxsw"
badFlags = Complement[flags, validFlags]

Assert[Length@badFlags == 0, "Invalid flags" <> StringJoin@badFlags];

If[ContainsAny[flags, {"b","B"}], Print["Braille not implemented"]];
printDebug = MemberQ[flags, "d"];
suppress = MemberQ[flags, "s"];
If[MemberQ[flags, "h"], Print["Print history not implemented"]];
wform = MemberQ[flags, "w"]
If[MemberQ[flags, "x"], Print["Explanation not implemented"]];

isVerbose = ContainsAny[flags, Characters@"vc"];
If[isVerbose && ContainsAny[flags, Characters@"fC"],
	Assert[False, "Cannot (de)compress already (de)compressed code!"]]



(* ::Subsection:: *)
(*Execution*)


isExecute = ContainsAny[flags, Characters@"fv"];

codeFile = $ScriptCommandLine[[3]];
rawCode = ReadString@codeFile;

code = Which[
	isVerbose, splitTokens@rawCode,
	wform, wToPostfix@ToExpression[rawCode, InputForm, HoldComplete],
	True, Assert[False, "Code not in verbose or Wolfram form"]];

(* compression should be reversible *)
Assert[decompress@compress@code == code];

debugHistory = {};

input = If[Length@$ScriptCommandLine >= 4,
	ReadList@$ScriptCommandLine[[4]],
	{}
];

Print@eval[code, input, wform]

If[printDebug,
	Print@code;
	Export["_tokenized.mx", {Length@tokenToBits@#, If[AssociationQ@#, Values@#,#]} &/@code, "Table"];
	Export["_compressed.mx", compressed = compress@code, "String"];
	Print["Compressed code exported to debug files"];
	codeLength = Length@compress@code;
	Print["Code length: ", codeLength, " bits = ", N@codeLength/8, " bytes"];
	Export["_debugHistory.mx", debugHistory // Map[OutputForm], "Table"] ]

