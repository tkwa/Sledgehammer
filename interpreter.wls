#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Subsection:: *)
(*Imports and setup*)


(* Get path whether run through a notebook or wolframscript -script *)
parentPath = $InputFileName /. "" :> NotebookFileName[];
parentDir = DirectoryName @ parentPath;
SetDirectory[parentDir];
Get["save.mx", Path -> Directory[]];
On[Assert]
printShows = False;
show := If[TrueQ[printShows], Echo, #&];
(*$IterationLimit = 2^14;*)

(* remove \[DownArrow] later when more builtins implemented *)
Unprotect@Slot; Slot[] := Slot[1]; Protect@Slot;


(* ::Subsection:: *)
(*Lexer*)


(* add syntax for argument order *)
(* Converts "nthOEIS.2" to <| "call" \[Rule] "nthOEIS", "arity" \[Rule] 2 *)
nameToToken[s_String] := Module[{parts = StringSplit[s,".",2], ret},
	Assert[Length@parts <= 2,"Malformed token"];
	<| "call" -> ToExpression[parts[[1]], StandardForm, Hold],
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

Assert[ {Hold[Map], 2} == Values@First@nextToken["Map.2  \"abc\"  MorphologicalComponents.1"]]
Assert["asdf\\jkkl" == First@nextToken["    \"asdf\\\\jkkl\" MorphologicalComponents.1   "]]
Assert[ <|"call"-> Hold@MorphologicalComponents, "arity"->1|>  == First@nextToken["MorphologicalComponents.1"]]

splitTokens =.
splitTokens[""] := {}
splitTokens[code_String] := Prepend[First@nextToken@code] @ splitTokens[ StringDrop[StringTrim@code, Last@nextToken@code]]

Assert[3 == Length@splitTokens["Map.2  \"a b  c\"  MorphologicalComponents.1"]]

show@"Lexer passed";


(* ::Subsection:: *)
(*Compression*)


(* ::Subsubsection:: *)
(*Token list constructor*)


show[#, "Token list succeeded, length = "]& @Length@allSHTokens; (* 8324 fixed, 6k more  *)


(* ::Subsubsection:: *)
(*Compressor*)


(*                     4    8    12    16     20    24 *)
show@"Starting compressor";
tokenDistribution = {
0, 0, 0, 0, 0, 5, 5, 5,
5, 4, 4, 4, 4, 4, 4, 4,
2, 1, 1, 1, 1, 1, 1}/64;

tokenCounts = MapIndexed[2^First@#2 * #1&, tokenDistribution];
Assert[Total@tokenDistribution <= 7/8];

cumulDistribution = Accumulate@tokenDistribution + 1/8;
cumulCounts = Accumulate@tokenCounts;

show@"Counts success";

idxToBits[idx_Integer] := Block[{nybs, startFrac,startCount,frac, verbose = False},
	nybs = 1 + Count[cumulCounts, _? (idx > # &)] // show[#,"nybs = "]&;
	startFrac = cumulDistribution[[nybs]] - tokenDistribution[[nybs]]// show[#,"startFrac = "]&;
	startCount = cumulCounts[[nybs]] - tokenCounts[[nybs]] + 1 // show[#,"startCount = "]&;
	frac = startFrac + (idx - startCount)*2^-nybs // show[#,"frac = "] &;
	First@RealDigits[frac, 2, nybs, -1]];

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

tokenToBits[tok_, SHTokens_List: allSHTokens] := idxToBits@First@FirstPosition[SHTokens, tok, $Failed, {1}];

compress[toks_List] := Join @@ Map[tokenToBits] @ toks;
compress[code_String] := compress @ splitTokens @ code;


postfixtoken[expr_] := Which[
	Depth@Head@expr == 2, <| "call" -> Head@expr, "arity" -> Length@expr |>,
	MatchQ[expr, Hold[_String] | Hold[_Integer]], ReleaseHold@expr,
	Depth@expr == 2, expr,
	True, Assert[False, "Compound heads not supported"]];

wToPostfix[expr_] := Map[postfixtoken, Level[
		Map[Hold, expr, {-1}, Heads -> True],
		{1,-2}]];

wToPostfix::usage = "Converts WL code HoldComplete[...] to postfix form, fails on compound heads";

(* test that a token compresses to a list *)
Assert[ListQ@tokenToBits[toToken[{MorphologicalPerimeter,2}]]]


show@"Compressor success";


(* ::Subsection:: *)
(*Decompression*)


(* bits starting with token code \[Rule] token, bits used *)
bitsToIdx[bits_List] := Module[{frac, startFrac, startCount, nybs, lenUsed},
	frac = FromDigits[bits, 2] / 2^(Length@bits);
	nybs = First@FirstPosition[cumulDistribution, _?(# > frac &)];
	lenUsed = Min[Length@bits, nybs];
	frac = FromDigits[Take[bits, UpTo[lenUsed]], 2] / 2^(lenUsed);
	startFrac = cumulDistribution[[nybs]] - tokenDistribution[[nybs]];
	startCount = cumulCounts[[nybs]] - tokenCounts[[nybs]] + 1;
	{startCount + (frac - startFrac) * 2^(nybs), lenUsed}]

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
Assert[AllTrue[ First@bitsToToken @ tokenToBits @# == # &][Map[toToken]@{{Map,2},{Times,3},{MorphologicalComponents,1}}]];

(* test that code is c and C correctly; test case includes int literals, str literals, functions *)
Assert[ (decompress@compress@# == splitTokens@# &)@"1337 42 Minus Times.2 3 IntegerDigits.2 \"abcde\" 0"];

(* same test with w code *)
fizzbuzz = HoldComplete@StringRiffle[Array[StringJoin[Pick[{"Fizz", "Buzz"}, Divisible[Slot[], IntegerDigits[35]]]] /. "" -> Slot[] & , 100]]
Assert[SameQ@@{#,decompress@compress@#}&@wToPostfix@fizzbuzz]


(* ::Subsection::Closed:: *)
(*Execution*)


(* add more types of literals later *)

applyToken =.
applyToken[tok_Integer | tok_String | tok_Slot] := (
	AppendTo[stack, tok];
	AppendTo[history, tok];
	If[printDebug, AppendTo[debugHistory,Activate@Map[HoldForm,stack]]];
)

applyToken[tok_Association] := Module[{call, arity, args, result},
	call = ReleaseHold@tok["call"];
	arity = tok["arity"];
	args = Take[stack, -arity];
	stack = Drop[stack, -arity];
	result = Apply[ Inactive@Evaluate@call, args ];

	AppendTo[stack, result];
	AppendTo[history, result];
	If[printDebug, AppendTo[debugHistory,Activate@Map[HoldForm,stack]]];
];

applyTokens[toks_List, args_List] := Block[{stack = args, history = args},
	Map[applyToken, toks];
	stack
];

(*SetOptions[eval, "printLast" \[Rule] True]*)
eval[toks_List, args_List, makeFunction_: False, OptionsPattern[]] := If[makeFunction,
	stack = applyTokens[toks, {}]; (Function@Activate@Last@stack) @@ args, 
	stack = applyTokens[toks, args]; Activate@Last@stack
];

evalVerbose[code_String, args_List] := eval[splitTokens@code, args];

(* when given as a Wolfram Language expression, it takes explicit arguments *)
evalW[code_String, args_List] := Function[evalVerbose[wToPostfix]][args];

(* We need this evalBits function for code-golf scoring purposes. Therefore, evalBits takes no options. *)
hammer[bits_List /; ContainsOnly[bits, {0,1}], args_List] := eval[decompress@bits, args];


(* ::Subsection:: *)
(*Testing...*)
