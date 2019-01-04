#!/usr/bin/env wolframscript
(* ::Package:: *)


version = "0.1.0"
time = SessionTime[];
parentPath = $InputFileName /. "" :> NotebookFileName[];
parentDir = DirectoryName @ parentPath;
SetDirectory[parentDir];

(* Gets argument pattern from symbol e.g. Map \[Rule] {_, _., _., OptionsPattern[]} *)
argPattern =.
argPattern[f_Symbol] := Lookup[SyntaxInformation@f, "ArgumentsPattern"];

(* e.g. Map \[Rule] Interval[1,4] *)
argInterval=.
argInterval[_Missing] := Interval[] (* non-function gives empty interval *)
argInterval[pats_List] := Count[pats, _List | _Blank | _BlankSequence] +
	Interval[{0,
		If[MemberQ[pats, _BlankNullSequence | _BlankSequence],
			Infinity,
		Count[pats, _Optional | _OptionsPattern]]}];

defaultArities = {0,1,2,3};
fixedArityForms =.
fixedArityForms[f_Symbol, possibleArities_List: defaultArities] :=
	Select[possibleArities, IntervalMemberQ[argInterval@argPattern@f, #]&] //
	Map[<|"call" ->  f, "arity" -> #|> &];
	
Assert[Length@fixedArityForms[Map,defaultArities] == 3];

systemSymbols = Map[Symbol,Names["System`*"]];
systemSymbols = Cases[systemSymbols, _Symbol];   (* only keep those that are actually symbols *)
fnSymbols = Select[systemSymbols, SyntaxInformation@# != {} &];

(* SystemInformation is the bottleneck *)

allSHTokens = Map[fixedArityForms[#, defaultArities]&, fnSymbols] // Catenate //
	SortBy[StringLength@SymbolName@#["call"]&];


DumpSave["save.mx", "Global`"];

Print[Length@allSHTokens, " token definitions saved"];
Print["Time: ", N[SessionTime[]-time]]



