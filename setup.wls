#!/usr/bin/env wolframscript
(* ::Package:: *)

version = "0.3.0";

time = SessionTime[];
parentPath = $InputFileName /. "" :> NotebookFileName[];
parentDir = DirectoryName @ parentPath;
SetDirectory[parentDir];
On@Assert

Print["Starting setup, version ", version, "..."];

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
			
argInterval[name_String] := argInterval @ argPattern @ Symbol@name;


(* Arities used for every function, including those with infinite args.
If a function accepts a bounded # of args, all will be included. *)
defaultArities = {0,1,2,3,4};

arities[f_Symbol, arities_List: defaultArities] := Block[{argint = argInterval@argPattern@f},
	Union[Select[arities, IntervalMemberQ[argint, #]& ],
		  If[Abs@Max@argint == Infinity, {}, Range@@MinMax@argint]]
];

fixedArityForms =.
fixedArityForms[name_String] :=
	call[name, #]& /@ arities@Symbol@name;

Assert[fixedArityForms["Map"] //
	Length@# == 4 && First@# === call["Map", 1] &];

Off[FrontEndObject::notavail, PersistenceLocation::invalid, Union::normal, NetFlatten::shdw, ResourceUpdate::shdw, $PublisherID::shdw];

Print["Obtaining frequency data from Wolfram servers..."]

namesfreqs = WolframLanguageData[All, {"Name","Frequencies"}] /. _Missing -> ("All" -> 10.^-8) // Map[First@# -> Lookup[Last@#, "All"]&] // Association;

names = namesfreqs // Keys;
Print[Length@names, " total names"];

headNames = names //
	Select[ToExpression[#, StandardForm, SyntaxInformation] != {} & ] //
	Select[Not@MissingQ@Lookup[ToExpression[#, StandardForm, SyntaxInformation], "ArgumentsPattern"]& ];

Print[Length@headNames, " symbols kept as possible heads"];

headSHTokens = headNames // Map[fixedArityForms] // Catenate;
Print[Length@headSHTokens, " head token definitions"];

literalSHTokens = Map[symbolLiteral, Union[Names["System`*"], Names["Combinatorica`*"]]] //
	Select[FreeQ[Attributes @@ #, Temporary]&];
Print[Length@literalSHTokens, " literal token definitions"]

(*orderedComplement[u_List, a_List] := Select[u, Not@MemberQ[a, #]&]; *)
(* Adjust token frequencies *)
toksfreqs = Join[headSHTokens, literalSHTokens] // Map[
	# -> Switch[#,
	_call, namesfreqs@#[[1]]^.6 * .1 * (#[[2]] /. {0->.5, 1->1, 2->1, 3->.5, 4->.25, _-> .5}),
	_symbolLiteral, namesfreqs[First@#] * .25 /. _Missing -> 10.^-8,
	_, Assert@False]& ];
	
(* Special-case literals *)
Print["Adding special cases for literals..."]
toksfreqs = Join[toksfreqs, { intLiteral[] -> .15, asciiLiteral[] -> .05 }];

Print["Arities: ", CountsBy[Join[headSHTokens, literalSHTokens], If[Head@# === call, #[[2]], "Literal"]& ]]


(* ::Subsubsection:: *)
(*Huffman encoding*)


Needs["Parallel`Queue`Priority`"];
Unprotect@Priority;Priority[r_Rule]:=-Values[r];

huffmanInit::usage = "Initializes a priority queue for Huffman encoding.";
huffmanInit[l_List] := Block[{q},
	q = priorityQueue[];
	Scan[EnQueue[q, #]&, l];
	q];

huffmanTree::usage = "Constructs Huffman tree from (token->weight) list.";
huffmanTree[l_List] := Block[{q},
	q = huffmanInit[l];
	Do[a = DeQueue[q]; b = DeQueue[q]; EnQueue[q, {Keys@a, Keys@b}-> Values@a + Values@b],
	Length@l - 1];
	First@First@Normal@q ];

(* The /. on the end is to save a sequence of 1s for EOF. *)
huffmanDict::usage = "Constructs compression table from (token->weight) list. All tokens must be depth 2.";
huffmanDict[l_List] := Association@Flatten@MapIndexed[# -> #2 - 1 &, huffmanTree@l, {-2}]/. x:{ Repeated[1]} :> RuleCondition@Append[x,0];



toktree = huffmanTree@toksfreqs;
tokToBit
tokToBitsDict = huffmanDict[toksfreqs];
bitsToTokDict = AssociationThread[Values@#, Keys@#]&@tokToBitsDict;


DumpSave["save.mx", "Global`"];

Print[Length@toksfreqs, " total token definitions saved"];
Print["Time: ", N[SessionTime[]-time]]
