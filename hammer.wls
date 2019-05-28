#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Subsection:: *)
(*Imports and setup*)


SetDirectory[DirectoryName[$InputFileName /. "" :> NotebookFileName[]]];
Get["save.mx", Path -> Directory[]];
With[{printShows = False}, Get["SHinterpreter`", Path -> Directory[]]];
On@Assert

flags  = $ScriptCommandLine[[2]] // Characters;
(* check flags *)

validFlags = Characters@"wcbdhxn";
badFlags = Complement[flags, validFlags];

Assert[Length@badFlags == 0, "Invalid flags" <> StringJoin@badFlags];

printDebug = MemberQ[flags, "d"];
If[MemberQ[flags, "h"], Print["Print history not implemented"]];
wform = MemberQ[flags, "w"];
cform = MemberQ[flags, "c"];
bform = MemberQ[flags, "b"];
If[MemberQ[flags, "x"], Print["Explanation not implemented"]];
noExecute = MemberQ[flags, "n"];

Assert[BooleanCountingFunction[1, 3][wform, cform, bform], "Must have exactly one of wcb flags!"];




(* ::Subsection:: *)
(*Execution*)


codeFile = $ScriptCommandLine[[3]];

code = Which[
	wform, ToExpression[ReadString@codeFile, InputForm, HoldComplete],
	cform, postfixToW@decompress@bytesToBits@BinaryReadList@codeFile,
	bform, postfixToW@decompress@brailleToBits@Import[codeFile,"Text", CharacterEncoding->"UTF8"],
	True, Assert[False, "Must have one of wcb flags!"]];

(* compression should be reversible *)
pfCode = wToPostfix@code;
Assert[decompress@padTo8@compress@pfCode == pfCode];

debugHistory = {};

input = If[Length@$ScriptCommandLine >= 4,
	ReadList@$ScriptCommandLine[[4]],
	{}
];

If[!noExecute, Print@eval[code, input]];

If[printDebug,
	Print@code;
	Export["_tokenized.mx", {Length@tokenToBits@#, If[AssociationQ@#, Values@#,#]} &/@code, "Table"];
	Export["_compressed.mx", compressed = compress@code, "String"];
	BinaryWrite["_.hmrc", bitsToBytes@compressed];
	WriteString[
		f = OpenWrite["_braille.hmrb", CharacterEncoding->"UTF-8"],
		bitsToBraille@compressed]; Close[f];
	Print["Compressed code exported to debug files"];
	codeLength = Length@compressed;
	Print["Code length: ", codeLength, " bits = ", N@codeLength/8, " bytes"];
	Export["_debugHistory.mx", debugHistory // Map[OutputForm], "Table"] ]

