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

bytesToBits = Map[IntegerDigits@#, 2]& /* Join

code = If[isVerbose,
	splitTokens@ReadString@codeFile,
	decompress@bytesToBits@BinaryReadList@codeFile];

(* compression should be reversible *)
Assert[decompress@compress@code == code];

codeLength = Length@compress@code
If[printDebug, Print["Code length: ", codeLength, " bits = ", N@codeLength/8, " bytes"]];
If[printDebug, Print["Tokenized code: ", code]];
isInput = Length@$ScriptCommandLine >= 4;
inputFile = If[isInput, $ScriptCommandLine[[4]], Null];
input = If[isInput, ReadList@inputFile, {}];

stack = input;
history = stack;

execute@code;
If[Length@stack>0,Activate@Last@stack // If[Not@suppress, Print@#]&];



