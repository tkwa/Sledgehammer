#!/usr/bin/env wolframscript
(* ::Package:: *)

version = "0.4.0";
(* Tested with Mathematica 12.0 *)

(* Additional packages to pull names from *)
packages = {"Combinatorica`", "Quaternions`", "FiniteFields`", "Experimental`"};

freqAdjustAssoc = {
	.005 -> {"Times","Power","Subtract","Minus",
	"s1","x1"},
	.001 -> {"Alternatives","Pattern","RGBColor","TwoWayRule",
	"s2","s3","x1","x2","x3", "Subscript", "CompoundExpression"},
	.0002 -> {"FinancialData", "GrayLevel", "Directive"}
} /. (x_ -> l_) :> Map[# -> x&,l] // Flatten // Association;

overrideArgPatterns = {"TwoWayRule" -> {_,_},"Span" -> {_, _, _.}, "Slot" -> {_}};

(* Arities used for functions that accept infinite args. *)
defaultArities = {0,1,2,3,4,5};

minFreq = 10.^-7;
(*  Adjust frequencies because code golf has more diversity of functions. 
0 = all tokens have same length; 1 = sample frequencies *)
fudgePower = .6;
arityFreqFactors = {0->.25, 1->1, 2->1, 3->.5, 4->.25, 5->.125, _-> .5};
symbolLiteralFreqFactor = .25;

(* These frequencies represent token frequencies rather than WL symbol frequencies. *)
specialCaseFreqs = { intLiteral[] -> .10, asciiLiteral[] -> .04, realLiteral[] -> .01};

time = SessionTime[];
parentPath = $InputFileName /. "" :> NotebookFileName[];
parentDir = DirectoryName @ parentPath;
SetDirectory[parentDir];
On@Assert

Print["Starting setup, version ", version, "..."];

Scan[Apply[(Unprotect@#; SyntaxInformation[Symbol@#] = {"ArgumentsPattern" -> #2})&],
	overrideArgPatterns] ;

(* Gets argument pattern from symbol e.g. Map \[Rule] {_, _., _., OptionsPattern[]} 
   Unfortunately need to evaluate the symbol, causing some errors. *)
argPattern =.
argPattern[f_Symbol] := Lookup[Once@SyntaxInformation@f, "ArgumentsPattern"];

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

arities[f_Symbol, arities_List: defaultArities] := Block[{argint = argInterval@argPattern@f},
	Union[Select[arities, IntervalMemberQ[argint, #]& ],
		  If[Abs@Max@argint == Infinity, {}, Range@@MinMax@argint]]
];

fixedArityForms =.
fixedArityForms[name_String] :=
	call[name, #]& /@ arities@Symbol@name;

Assert[fixedArityForms["Map"] === Map[call["Map",#]&,Range@4]];



Off[FrontEndObject::notavail];

Print["Obtaining frequency data from Wolfram servers..."];

namefreqs = Once[WolframLanguageData[All, {"Name","Frequencies"}], "Local", PersistenceTime -> 86400] /. _Missing -> {} //
	Map[First@# -> Lookup[#[[2]], "All", minFreq]& ] // Association;

Print[Length@namefreqs, " total names from WolframLanguageData"];

AssociateTo[namefreqs, freqAdjustAssoc];

experimentals = EntityValue[EntityClass["WolframLanguageSymbol", "Experimental"],{"Name","VersionIntroduced"}] //
	Select[#[[2]]>= 12&] // Map[First];


names = namefreqs // Keys (*// Complement[#,experimentals]&*);
Print[Length@names, " non-experimental names kept"];
(* Adjust frequencies of tokens that are either too common or too uncommon *)


headNames = names //
	Complement[#, EntityValue[EntityClass["WolframLanguageSymbol","Autoevaluating"],"Name"]]& //
	Map[# -> ToExpression[#,StandardForm, Once@*SyntaxInformation]&] //
	Select[#[[2]] != {} & ] //
	Select[! MissingQ@Lookup[#[[2]],"ArgumentsPattern"]& ] //
	Map[First];

Print[Length@headNames, " symbols kept as possible heads"];

headSHTokens = headNames // Map[fixedArityForms] // Catenate;
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

tokfreqs = Join[headSHTokens, literalSHTokens] // Map[
	# -> Switch[#,
	_call, namefreqs@#[[1]]^fudgePower * (#[[2]] /. arityFreqFactors),
	_symbolLiteral, namefreqs[First@#]^fudgePower * symbolLiteralFreqFactor /. _Missing -> minFreq,
	_, Assert@False]& ];


(* Normalize token frequencies *)
total = Total@Values@tokfreqs;
tokfreqs = MapAt[# / total &, tokfreqs, {All,2}];

(* Special-case literals *)
Print["Adding special cases for literals..."];
tokfreqs = Join[tokfreqs, specialCaseFreqs];

Print["Sum of freqs: ", total, "; ", "Arities: ", CountsBy[Join[headSHTokens, literalSHTokens], If[Head@# === call, #[[2]], "Literal"]& ] // KeySort];
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
huffmanDict[l_List] := Association@Flatten@MapIndexed[# -> #2 - 1 &, huffmanTree@l, {-2}] /.
	x:{ Repeated[1]} :> RuleCondition@Append[x,0];



Print["Constructing Huffman tree..."]

(*toktree = huffmanTree@toksfreqs;*)
tokToBitsDict = huffmanDict[tokfreqs];


Put[tokToBitsDict // Map[FromDigits[Prepend[#,1],2]&],"save.mx"]

Print[Length@tokfreqs, " total token definitions saved"];
Print["Time: ", N[SessionTime[]-time]]
Quit[]










