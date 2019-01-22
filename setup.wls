#!/usr/bin/env wolframscript
(* ::Package:: *)

 (* version 0.1.2 *)
time = SessionTime[];
parentPath = $InputFileName /. "" :> NotebookFileName[];
parentDir = DirectoryName @ parentPath;
SetDirectory[parentDir];
On@Assert

Print@"Starting setup...";

toToken[tok_List /; Length@tok == 2] := Association[ "call" -> Hold@Evaluate@tok[[1]], "arity" -> tok[[2]] ];

(* Gets argument pattern from symbol e.g. Map \[Rule] {_, _., _., OptionsPattern[]} *)
argPattern =.
argPattern[f_Symbol] := Lookup[SyntaxInformation@f, "ArgumentsPattern"];

(* e.g. Map \[Rule] Interval[1,4] *)
argInterval=.
argInterval[_Missing] := Interval[] (* non-function gives empty interval *)
argInterval[pats_List] := 
	Count[pats, _List | _Blank | _BlankSequence] +
		Interval[{0,
			If[MemberQ[pats, _BlankNullSequence | _BlankSequence],
				Infinity,
			Count[pats, _Optional | _OptionsPattern]]}];
			
argInterval[f_Symbol] := argInterval @ argPattern @ f;


(* Arities used for every function, including those with infinite args.
If a function accepts a bounded # of args, all will be included. *)
defaultArities = {0,1,2,3,4};

SHArities[f_Symbol, arities_List: defaultArities] := Block[{argint = argInterval@f},
	Union[Select[arities, IntervalMemberQ[argint, #]& ],
		  If[Abs@Max@argint == Infinity, {}, Range@@MinMax@argint]]
];

fixedArityForms =.
fixedArityForms[f_Symbol] :=
	SHArities[f] // Map[<|"call" ->  Hold@Evaluate@f, "arity" -> #|> &];

Assert[fixedArityForms[Map] //
	Length@# == 4 && First@# == <| "call" -> Hold[Map], "arity" -> 1 |> &];


Off[FrontEndObject::notavail, PersistenceLocation::invalid, Union::normal, NetFlatten::shdw, ResourceUpdate::shdw, $PublisherID::shdw];

Print["Obtaining frequency data from Wolfram servers..."]

namesfreqs = WolframLanguageData[All, {"Name","Frequencies"}] /. _Missing -> ("All" -> 10.^-8) // Map[First@# -> Lookup[Last@#, "All"]&] // Association;
Print["Sorting tokens by frequency..."]

names = namesfreqs // Keys;
Print["Tokens sorted"]

symbols = Map[ToExpression[#, StandardForm, Hold]&, names];

Print[Length@symbols, " total names"];

headSymbols = symbols //
	Select[SyntaxInformation@ReleaseHold@# != {} &] //
	Select[Not@MissingQ@Lookup[SyntaxInformation@ReleaseHold@#, "ArgumentsPattern"]&];

Print[Length@headSymbols, " symbols kept as possible heads"];

headSHTokens = headSymbols // Map[fixedArityForms @ ReleaseHold@#&] // Catenate;
Print[Length@headSHTokens, " head token definitions"];

literalSHTokens = Map[ToExpression[#, StandardForm, Hold]&, Names@"System`*"] //
	Select[FreeQ[Attributes @@ #, Temporary]&];
Print[Length@literalSHTokens, " literal token definitions"]

(*orderedComplement[u_List, a_List] := Select[u, Not@MemberQ[a, #]&];*)

allSHTokens = Join[headSHTokens, literalSHTokens] // SortBy[-
	If[AssociationQ@#, namesfreqs[#]["call"]]&];

Print["Arities: ", CountsBy[allSHTokens, If[AssociationQ@#, #["arity"], "Literal"]& ]]


DumpSave["save.mx", "Global`"];

Print[Length@allSHTokens, " total token definitions saved"];
Print["Time: ", N[SessionTime[]-time]]







