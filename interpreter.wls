#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Subsection:: *)
(*Imports and setup*)


parentPath = $InputFileName /. "" :> NotebookFileName[]
parentDir = DirectoryName @ parentPath;
SetDirectory[parentDir];
Get["save.mx", Path -> Directory[]];
On[Assert]
printShows = False;
show := If[TrueQ[printShows], Echo, #&];
$IterationLimit = 2^14;


(* ::Subsection:: *)
(*Lexer*)


(* add syntax for argument order *)
(* Converts "nthOEIS.2" to <| "call" \[Rule] nthOEIS, "arity" \[Rule] 2 *)
nameToToken[s_String] := Module[{parts = StringSplit[s,".",2], ret},
	Assert[Length@parts <= 2,"Malformed token"];
	<| "call" -> Symbol@parts[[1]],
		"arity" -> If[Length@parts == 1, 1, ToExpression@parts[[2]]] |>
	];

(* input is position of the end of a token
return {next token, rest of length used} *)
nextToken =.
nextToken[code_String] := Module[{trimmedCode = StringTrim[code]},
	If[StringTake[trimmedCode, 1] == "\"",           (* string literal *)
		First@Select[Array[ StringTake[trimmedCode, #] &, StringLength@trimmedCode],
			SyntaxQ,
			1] // {ToExpression@#, StringLength@# }&,
		StringSplit[trimmedCode, Whitespace, 2] // 
			If[StringMatchQ[First@#, DigitCharacter..],   
				{ToExpression@First@#, StringLength@First@#},  (* integer literal *)
				{nameToToken@First@#, StringLength@First@#} ]& (* function *)
]];

Assert[ {Map, 2} == Values@First@nextToken["Map.2  \"abc\"  MorphologicalComponents.1"]]
Assert["asdf\\jkkl" == First@nextToken["    \"asdf\\\\jkkl\" MorphologicalComponents.1   "]]
Assert[ <|"call"->MorphologicalComponents, "arity"->1|>  == First@nextToken["MorphologicalComponents.1"]]

splitTokens =.
splitTokens[""] := {}
splitTokens[code_String] := Prepend[First@nextToken@code] @ splitTokens[ StringDrop[StringTrim@code, Last@nextToken@code]]

Assert[3 == Length@splitTokens["Map.2  \"a b  c\"  MorphologicalComponents.1"]]

show@"Lexer passed";


(* ::Subsection:: *)
(*Compression*)


(* ::Subsubsection:: *)
(*Token list constructor*)


h[n_Integer] := Part[history, -n];

PrependTo[allSHTokens, <| call -> Slot, arity -> 1 |>];
PrependTo[allSHTokens, <| call -> h, arity -> 1 |>];

show[#, "Token list succeeded, length = "]& @Length@allSHTokens; (* 8324 fixed, 6k more  *)


(* ::Subsubsection:: *)
(*Compressor*)


(*                     4    8    12    16     20    24 *)
show@"Starting compressor";
tokenDistribution = {1/16, 3/8, 3/16, 3/16, 1/32, 1/64};

tokenCounts = MapIndexed[16^First@#2 * #1&, tokenDistribution];
Assert[Total@tokenDistribution <= 7/8];

cumulDistribution = Accumulate@tokenDistribution + 1/8;
cumulCounts = Accumulate@tokenCounts;

show@"Counts success";

idxToBits[idx_Integer] := Block[{nybs, startFrac,startCount,frac, verbose = False},
	nybs = 1 + Count[cumulCounts, _? (idx > # &)] // show[#,"nybs = "]&;
	startFrac = cumulDistribution[[nybs]] - tokenDistribution[[nybs]]// show[#,"startFrac = "]&;
	startCount = cumulCounts[[nybs]] - tokenCounts[[nybs]] + 1 // show[#,"startCount = "]&;
	frac = startFrac + (idx - startCount)*16^-nybs // show[#,"frac = "] &;
	First@RealDigits[frac, 2, 4 * nybs, -1]];

toAssociation[tok_List] := Association[ "call" -> tok[[1]], "arity" -> tok[[2]] ];

tokenToBits[tok_Association, SHTokens_List: allSHTokens] := idxToBits@First@FirstPosition[SHTokens, tok];

eliasGamma[0] := {1}
eliasGamma[n_Integer /; n > 0] := IntegerDigits[n,2] // Join[ConstantArray[0,Length@# - 1], #]&;

eliasDelta[n_Integer /; n > 0] := IntegerDigits[n,2] // Join[eliasGamma[Length@#], Rest@# ]&;

eliasDelta[_] := Throw@"Argument to Elias Delta must be a positive integer."

varEliasDelta[n_Integer /; n >= 0, k_Integer: 3] := Join[eliasDelta[Floor[n/2^k]+1], IntegerDigits[n,2,k]];

(* modified Elias Delta, k=8 *)
tokenToBits[n_Integer /; n >= 0] := varEliasDelta[n,3] //
	Join[{0,0,0,0},#]&;

(* packed into 7 bits *)
tokenToBits[str_String /; Max@ToCharacterCode@str <= 127] := Module[{len = StringLength@str},
	ToCharacterCode@str // IntegerDigits[#, 2, 7]& // Flatten //
	Join[{0,0,0,1,0,0},varEliasDelta[len],#] &
];

compress[toks_List] := Join @@ Map[tokenToBits] @ toks;
compress[code_String] := compress @ splitTokens @ code;

(* test that a 1.5-byte token compresses correctly *)
Assert[idxToBits[98] == First@RealDigits[9/16, 2, 4*3, -1]];

(* test that a token compresses to a list *)
Assert[ListQ@show@tokenToBits[toAssociation[{MorphologicalPerimeter,2}]]]


show@"Compressor success";


(* ::Subsection:: *)
(*Decompression*)


(* bits starting with token code \[Rule] token, bits used *)
bitsToIdx[bits_List] := Module[{frac, startFrac, startCount, nybs, lenUsed},
	frac = FromDigits[bits, 2] / 2^(Length@bits);
	nybs = First@FirstPosition[cumulDistribution, _?(# > frac &)];
	lenUsed = Min[Length@bits, 4 nybs];
	frac = FromDigits[Take[bits, UpTo[lenUsed]], 2] / 2^(lenUsed);
	startFrac = cumulDistribution[[nybs]] - tokenDistribution[[nybs]];
	startCount = cumulCounts[[nybs]] - tokenCounts[[nybs]] + 1;
	{startCount + (frac - startFrac) * 16^nybs, lenUsed}]

(* returns n, length used *)
unEliasGamma[bits_List] := Module[{leadingZeros = Count[First@Split@bits,0]},
	{FromDigits[bits[[leadingZeros + 1;; 2 leadingZeros + 1]],2],
	2 leadingZeros + 1}
];

(* returns n, length used *)
unEliasDelta[bits_List] := Module[{lennp1, lenUsed},
	{lennp1, lenUsed} = unEliasGamma[bits];
	{FromDigits[ Prepend[1]@ bits[[lenUsed + 1 ;; lenUsed + lennp1 - 1]], 2], lenUsed + lennp1 - 1}
];

unVarEliasDelta[bits_List, k_Integer:3] := Module[{ndiv8p1, lenUsed},
	{ndiv8p1, lenUsed} = unEliasDelta[bits];
	{(ndiv8p1 - 1) * 2^k + FromDigits[ bits[[lenUsed+1 ;; lenUsed+k]], 2], lenUsed + k}
];

(* integer literal. Returns n, length used *)
bitsToToken[{0,0,0,0,b___}] := Module[{bits = List@b, ndiv8p1, lenUsed},
	unVarEliasDelta[bits, 3] // {First@#, 4+Last@#}&
];

(* string literal. *)
bitsToToken[{0,0,0,1,0,0,b___}] := Module[{bits = List@b, strLen, lenLen},
	{strLen, lenLen} = unVarEliasDelta[bits, 3];
	Drop[bits, lenLen] //
	Take[#,7 * strLen]& //
	Partition[#,7]& //
	Map[FromDigits[#,2] &] //
	FromCharacterCode //
	{#, 6 + lenLen + 7 * strLen} &
];

(* string literal *)
bitsToToken[{0,0,0,1,0,1,b___}] := Module[{bits = List@b},
	{"Non-ASCII string decoding not implemented..." // Echo,7}];

(* other literal *)
bitsToToken[{0,0,0,1,___}] :=
	{"Unimplemented literal",4};

(* returns token, length used *)
bitsToToken[bits_List, SHTokens_List: allSHTokens] := Module[{idx, lenUsed},
	{idx, lenUsed} = bitsToIdx@bits;
	{Part[SHTokens, idx], lenUsed}
];

(* returns list of tokens in association form *)
decompress[{}] := {};
decompress[bits_List] := Module[{tok, lenUsed},
	{tok, lenUsed} = bitsToToken[bits];
	Prepend[tok][decompress[Drop[bits, lenUsed]]]
];


(* test that integer literals are compressed and decompressed correctly, with correct length *)
Assert[{1337,21} == bitsToToken@tokenToBits[1337]];
Assert[AllTrue[ First@bitsToToken @  tokenToBits@# == # &][{0,1,2,42,96,97,1337,24601}]];

(* test that string literals are compressed and decompressed correctly, with correct length *)
Assert[AllTrue[bitsToToken @ tokenToBits @ # == {#,Length@tokenToBits@#}&][{"Hello, World!"}]];

(* test that integer literals before the end of a bitstring are compressed correctly *)
Assert[AllTrue[ First@bitsToIdx @ Join[#,{1,0,1,0,1,0,1}]& @ idxToBits @# == # &][{1, 2, 42, 96, 97, 1337, 24601}]];

(* test that a sequence of tokens is compressed and decompressed correctly *)
Assert[AllTrue[ First@bitsToToken @ tokenToBits @# == # &][Map[toAssociation]@{{Map,2},{Times,3},{MorphologicalComponents,1}}]];

(* test that code is c and C correctly; test case includes int literals, str literals, functions *)
Assert[ (decompress@compress@# == splitTokens@# &)@"1337 42 Minus Times.2 3 IntegerDigits.2 \"abcde\" 0"];


(* ::Subsection:: *)
(*Execution*)


(* add more types of literals later *)

applyToken =.
applyToken[tok_Integer | tok_String | tok_Slot] := (
	AppendTo[stack, tok];
	AppendTo[history, tok];
	If[printDebug, Echo@Activate@Map[Hold,stack]];)

applyToken[tok_Association] := Module[{call, arity, args, result},
	call = tok["call"];
	arity = tok["arity"];
	args = Take[stack, -arity];
	stack = Drop[stack, -arity];
	result = Apply[ Inactive@Evaluate@tok["call"], args ];

	AppendTo[stack, result];
	AppendTo[history, result];
	If[printDebug, Echo@Activate@Map[Hold,stack]];
];

execute[toks_List] := Map[applyToken,toks];
executeVerbose[code_String] := execute @ splitTokens @ code;
executeBinary[bits_List] := execute @ decompress @ bits;


(* ::Subsection:: *)
(*Testing...*)






