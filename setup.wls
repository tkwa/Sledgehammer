#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Tested with Mathematica 12.0 *)
version = "0.5.0-alpha.0";

SetDirectory[DirectoryName[$InputFileName /. "" :> NotebookFileName[]]];
(* Additional packages to pull names from. Currently only used for symbol literals. *)
packages = {"Combinatorica`", "Quaternions`", "FiniteFields`", "Experimental`", "SHUtils`"};

(* Applied to names, not tokens *)
freqAdjustAssoc = {
	.02 -> {"List", "Rule"},
	.005 -> {"Times","Power","Subtract","Minus",
	"s1","x1"},
	.001 -> {"Alternatives","Pattern","RGBColor","TwoWayRule",
	"s2","s3","ss1","x2","x3","Subscript", "CompoundExpression"},
	.0002 -> {"FinancialData", "GrayLevel", "Directive"}~Join~("x" <> ToString@#& /@ Range[4,256])
} /. (x_ -> l_) :> Map[# -> x&,l] // Flatten // Association;

(* Arities used for functions that accept infinite args. *)
defaultArities = {0,1,2,3,4,5,6,7};

(* Tokens to manually set arity *)
arityOverrides = {
	"Alternatives" -> Range[0, 16],
	"Association" -> Range[0,16],
	"Break" -> {0,1}, (* 1-argument form could be deprecated? *)
	"CompoundExpression" -> Range[1, 64],
	"FindPermutation" -> {1},
	"Inequality" -> Range[0, 16],
	"K" -> Range[0,7],
	"KeyValueMap" -> {1,2},
	"List" -> Range[0, 64],
	"MessageName" -> {2},
	"Now" -> {0,1},
	"Part" -> Range[1,16],
	"Plus" -> Range[0,32],
	"RuleCondition" -> {1},
	"RunScheduledTask" -> {1,2}, (**)
	"Slot" -> {1},
	"Span" -> {2,3},
	"StringJoin" -> Range@16,
	"Switch" -> Range@16,
	"Times" -> Range[0,16],
	"Today" -> {0,1},
	"TwoWayRule" -> {2},
	"URLFetch" -> {1,2,3}, (* Valid SyntaxInformation, but not in WLD! Fix this! *)
	"Which" -> Range@16
	} //
	 Join[#, Map[("x" <> ToString@#) -> Range[0,8 + 8*Boole[# <= 16]] &, Range[0,64]]]&;

(*  Adjust frequencies because code golf has more diversity of functions. 
0 = all tokens have same length; 1 = sample frequencies *)
minFreq = 1/2^17.; (* 19-20 bits *)
fudgePower = .6;

(* Factors multiplied with WLD frequency if a token is not found in training data. *)
arityFreqFactors = {0->.25, 1->1, 2->1, 3->.5, 4->.25, 5->.125, 6->.0625, x_Integer -> .02, _ -> .02} //
	Association // .05*#& // Normal;
symbolLiteralFreqFactor = .4*.05;

(* These frequencies represent token frequencies rather than WL symbol frequencies. *)
SetDirectory[DirectoryName[$InputFileName /. "" :> NotebookFileName[]]];
modelFreqs =  Get["modelfreqs.mx"] // KeyDrop["Stop"];
specialCaseFreqs = <| |>;

time = SessionTime[];
parentPath = $InputFileName /. "" :> NotebookFileName[];
parentDir = DirectoryName @ parentPath;
SetDirectory[parentDir];
On@Assert

Print["Starting setup, version ", version, "..."];

Scan[Apply[(arities[#1] = #2)&],
	arityOverrides];

(* Gets argument pattern from symbol e.g. Map \[Rule] {_, _., _., OptionsPattern[]} 
   Unfortunately need to evaluate the symbol, causing errors on autoevaluating symbols. *)
argPattern =.;
argPattern[f_Symbol] := Lookup[Once@SyntaxInformation@f, "ArgumentsPattern"];

(* e.g. Map \[Rule] Interval[1,4] 
to allow SlotSequence to replace a pair of args, adjust minimum args with 0\[Rule]0, 1\[Rule]1, 2\[Rule]1, ...
Also allow a maximum of 2 options *)
argInterval=.
argInterval[_Missing] := Interval[] (* non-function gives empty interval *)
argInterval[pats_List] := Module[{p = pats /. HoldPattern[Pattern][_,pat_] :> pat, blankcount},
	blankcount = Count[p, _List | _Blank | _BlankSequence];
	Interval[{blankcount - Boole[blankcount >= 2],
		If[MemberQ[p, _BlankNullSequence | _BlankSequence],
			Infinity,
			blankcount + Count[p, _Optional] + 2 Count[p,  _OptionsPattern]]}]
];
argInterval[name_String] := argInterval @ argPattern @ Symbol@name;

arities[name_String, arities_List: defaultArities] := Block[{argint = argInterval@argPattern@Symbol@name},
	Union[Select[arities, IntervalMemberQ[argint, #]& ],
		  If[Abs@Max@argint == Infinity, {}, Range@@MinMax@argint]]
];

legalCallForms =.
legalCallForms[name_String] :=
	call[name, #]& /@ arities@name;

Assert[legalCallForms["Map"] === Map[call["Map",#]&,Range@5]];


Off[FrontEndObject::notavail];

Print["Obtaining frequency data from Wolfram servers..."];

namefreqs = Once[WolframLanguageData[All, {"Name","Frequencies"}], "Local", PersistenceTime -> 86400] /. _Missing -> {} //
	Map[First@# -> Lookup[#[[2]], "All", minFreq]& ] // Association // Map[Max[#, minFreq] ^ fudgePower & ];

Print[Length@namefreqs, " total names from WolframLanguageData"];

AssociateTo[namefreqs, freqAdjustAssoc];

(* experimentals = EntityValue[EntityClass["WolframLanguageSymbol", "Experimental"],{"Name","VersionIntroduced"}] //
	Select[#[[2]]>= 12&] // Map[First]; *)


names = namefreqs // Keys (*// Complement[#,experimentals]&*);
Print[Length@names, " non-experimental names kept"];
(* Adjust frequencies of tokens that are either too common or too uncommon *)


headNames = names //
	Complement[#, EntityValue[EntityClass["WolframLanguageSymbol","Autoevaluating"],"Name"]]& //
	Map[# -> ToExpression[#,StandardForm, Once@*SyntaxInformation]&] //
	Select[#[[2]] != {} & ] //
	Select[! MissingQ@Lookup[#[[2]],"ArgumentsPattern"]& ] //
	Map[First] //
	Join[#, First /@ arityOverrides]&;

Print[Length@headNames, " symbols kept as possible heads"];

headSHTokens = headNames // Map[legalCallForms] // Catenate;
Print[Length@headSHTokens, " head token definitions"];

Print["Loading additional packages..."];

Scan[Needs, packages];
(* remove temporary system variables, add all  *)
literalSHTokens = Map[Names,Join[{"System`*"},Map[#<> "*"&, packages]]] // Flatten //
	Join[#,Keys@freqAdjustAssoc]& //
	Select[FreeQ[Attributes @@ #, Temporary]&] // Map[symbolLiteral];

Print[Length@literalSHTokens, " literal token definitions"];

(*orderedComplement[u_List, a_List] := Select[u, Not@MemberQ[a, #]&]; *)
(* Adjust token frequencies *)

tokfreqs = Join[headSHTokens, literalSHTokens] // AssociationMap[
	Switch[#,
	_call, namefreqs[First@#]* (#[[2]] /. arityFreqFactors) /. _Missing -> minFreq,
	_symbolLiteral, namefreqs[First@#] * symbolLiteralFreqFactor /. _Missing -> minFreq,
	_, Assert@False]& ];


(* Normalize token frequencies *)

(* Join keeps the last value, so frequencies in training data are kept *)
tokfreqs = Join[tokfreqs, modelFreqs];
Print[Length@modelFreqs, " freqencies added from model"];

total = Total@Values@tokfreqs;
tokfreqs = tokfreqs / total;

(* Special-case literals *)
Print["Adding special cases for literals..."];
tokfreqs = Join[tokfreqs, specialCaseFreqs];

Print["Sum of freqs: ", total];
Print["Arities: ", CountsBy[Keys@tokfreqs, If[Head@# === call, #[[2]], "Literal"]& ] // KeySort];
(* //RuntimeTools`Profile; *)


(* ::Subsubsection:: *)
(*Huffman encoding*)


Needs["Parallel`Queue`Priority`"];
Unprotect@Priority;Priority[r_Rule]:=-Values[r];

(* https://mathematica.stackexchange.com/a/31976/61597 *)

huffmanInit::usage = "Initializes a priority queue for Huffman encoding.";
huffmanInit[l_List] := Block[{q},
	q = priorityQueue[];
	Scan[EnQueue[q, #]&, l];
	q
];

huffmanTree::usage = "Constructs Huffman tree from (token->weight) list.";
huffmanTree[l_List] := Block[{q,a,b},
	q = huffmanInit[l];
	Do[a = DeQueue[q]; b = DeQueue[q]; EnQueue[q, {Keys@a, Keys@b}-> Values@a + Values@b],
	Length@l - 1];
	First@First@Normal@q
];

(* The /. on the end is to save a sequence of 1s for EOF. *)
huffmanDict::usage = "Constructs compression table from (token->weight) list. All tokens must be depth 2.";
huffmanDict[l_List] := Block[{},
	Association@Flatten@MapIndexed[# -> #2 - 1 &, huffmanTree@l, {-2}] /.
		x:{ Repeated[1]} :> RuleCondition@Append[x,0]
];
huffmanDict[a_Association] := Block[{},
	If[AnyTrue[Keys@a, Depth@# != 2 &], Throw["Tokens must be depth 2!"]];
	huffmanDict@Normal@a
];


Print["Constructing Huffman tree..."]

(*toktree = huffmanTree@toksfreqs;*)
tokToBitsDict = huffmanDict[tokfreqs];


Put[tokToBitsDict // Map[FromDigits[Prepend[#,1],2]&],"compression_dict.mx"]

Print[Length@tokToBitsDict, " total token definitions saved"];
Print["Time: ", N[SessionTime[]-time]]

Quit[]






