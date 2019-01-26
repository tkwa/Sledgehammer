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
(*Compression*)


show[#, "Token list length = "]& @Length@tokToBitsDict; (* 8324 fixed, 6k more  *)


(* ::Subsubsection:: *)
(*Compressor*)


eliasGamma[0] := {1}
eliasGamma[n_Integer /; n > 0] := IntegerDigits[n,2] // Join[ConstantArray[0,Length@# - 1], #]&;

eliasDelta[n_Integer /; n > 0] := IntegerDigits[n,2] // Join[eliasGamma[Length@#], Rest@# ]&;

eliasDelta[_] := Throw@"Argument to Elias Delta must be a positive integer."

varEliasDelta[n_Integer /; n >= 0, k_Integer: 3] := Join[eliasDelta[Floor[n/2^k]+1], IntegerDigits[n,2,k]];

(* modified Elias Delta, k=8 *)
tokenToBits[intLiteral[n_] /; n >= 0] := Join[tokenToBits@intLiteral[],varEliasDelta[n]];

(* ASCII strings packed into 7 bits per character.*)
tokenToBits[asciiLiteral[str_] /; Max@ToCharacterCode@str <= 127] := Module[{len},
	len = StringLength@str;
	ToCharacterCode@str // IntegerDigits[#, 2, 7]& // Flatten //
	Join[tokenToBits@asciiLiteral[],varEliasDelta[len],#] &
];

tokenToBits[tok_, encodeDict_: tokToBitsDict] := Lookup[ encodeDict, tok, Assert[False, {"Token not found!",tok}]];

compress[toks_List] := Join @@ Map[tokenToBits] @ toks /. {a___, 1...} :> {a};


(* ::Subsubsection:: *)
(*Lexer/parser*)


ClearAttributes[symbolLiteral, HoldFirst]

postfixtoken[expr_] := Module[{h, name}, Which[
	Depth@Head@expr == 2, call[SymbolName@@Unevaluated/@Head@expr,Length@expr],
	MatchQ[expr, Hold[_String]], asciiLiteral@@expr,
	MatchQ[expr, Hold[_Integer]], intLiteral@@expr,
	Depth@expr == 2, symbolLiteral[SymbolName@@Unevaluated/@expr],
	True, Assert[False, "Compound heads not supported"]]];

wToPostfix[expr_] := Map[postfixtoken, Level[
		Map[Hold, expr, {-1}, Heads -> True],
		{1,-2}]];

wToPostfix::usage = "Converts WL code HoldComplete[...] to postfix form, fails on compound heads";




(* ::Subsection:: *)
(*Decompression*)


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

(* returns integer, length used *)
unVarEliasDelta::usage = "Decode a variant Elias Delta bitstring";
unVarEliasDelta[bits_List, k_Integer:3] := Module[{ndiv8p1, lenUsed},
	{ndiv8p1, lenUsed} = unEliasDelta[bits];
	{(ndiv8p1 - 1) * 2^k + FromDigits[ bits[[lenUsed+1 ;; lenUsed+k]], 2], lenUsed + k}
];

(* ASCII literal. *)
decodeASCIILiteral[bits_] := Module[{strLen, lenLen},
	{strLen, lenLen} = unVarEliasDelta[bits, 3];
	Drop[bits, lenLen] //
	Take[#, 7 * strLen]& //
	Partition[#,7]& //
	Map[FromDigits[#,2] &] //
	FromCharacterCode //
	{#, lenLen + 7 * strLen} &
];

(* returns token, length used.*)
bitsToToken[bits_List, decodeDict_: bitsToTokDict] := Module[{pfx, lenUsed, tok},
	(* try increasing prefixes until one is a key of decodeDict *)
	For[lenUsed = 0, lenUsed < 32 && !KeyMemberQ[decodeDict, pfx = Take[bits, lenUsed]], lenUsed++, Null];
	tok = decodeDict[pfx];
	Switch[tok,
		_call , {tok, lenUsed},
		_symbolLiteral , {tok, lenUsed},
		_intLiteral , {intLiteral[#], lenUsed + #2}& @@ unVarEliasDelta[Drop[bits, lenUsed]],
		_asciiLiteral , {asciiLiteral[#], lenUsed + #2}& @@ decodeASCIILiteral[Drop[bits, lenUsed]]
	]
];

(* returns list of tokens *)
decompressNoPad[{}] := {}
decompressNoPad[{Repeated[1]}] := {};

decompressNoPad[bits_List] := Module[{tok, lenUsed},
	{tok, lenUsed} = bitsToToken[bits];
	Prepend[tok][decompressNoPad[Drop[bits, lenUsed]]]];
	
decompress[bits_List] := decompressNoPad[ArrayPad[bits, {0,32},1]];


(* ::Subsection:: *)
(*Converting between Braille, binary file, and compressed forms*)


padTo8[bits_List] := PadRight[bits /. {a___, Repeated[1]} :> {a}, Ceiling[Length@bits,8],1];
partition8[bits_List] := Partition[padTo8@bits, 8];

bitsToBytes[bits_List] := Map[FromDigits[#, 2]&, partition8@bits];
bytesToBits[bytes_List] := Flatten@Map[IntegerDigits[#,2,8]&, bytes]

bitsToBraille[bits_List] := StringJoin@Map[FromCharacterCode[16^^2800 + FromDigits[Permute[#,{8,7,6,2,5,4,3,1}],2],"Unicode"]&,partition8@bits];
brailleToBits[brs_String] := ToCharacterCode[brs, "Unicode"] - 16^^2800 // Map[Permute[IntegerDigits[#,2,8],InversePermutation@{8,7,6,2,5,4,3,1} ]&] // Flatten;

(*decompress@brailleToBits@bitsToBraille@compress@wToPostfix@fizzbuzz*)


(* test that integer literals are compressed and decompressed correctly, with correct length *)
Assert[AllTrue[ First@bitsToToken @  tokenToBits@# == # &][ intLiteral /@ {0,1,2,42,96,97,1337,24601}]];

(* test that string literals are compressed and decompressed correctly, with correct length *)
Assert[AllTrue[bitsToToken @ tokenToBits @ # == {#,Length@tokenToBits@#}&][asciiLiteral /@ {"Hello, World!"}]];

(* test that an expression decompresses even after appending 32 1s *)
fizzbuzz = HoldComplete@StringRiffle[Array[List@StringJoin[Pick[{"Fizz", "Buzz"}, Divisible[Slot[], {3,5}]]] /. "" -> Slot[] & , 100]];
Assert[SameQ@@{#,decompress@compress@#}&@wToPostfix@fizzbuzz]


(* ::Subsection:: *)
(*Execution*)


(* add more types of literals later *)

applyToken =.
applyToken[tok_intLiteral | tok_asciiLiteral ] := (
	AppendTo[stack, First@tok];
	AppendTo[history, First@tok];
	If[printDebug, AppendTo[debugHistory,Activate@Map[HoldForm,stack]]];
)

applyToken[tok_call] := Module[{f, arity, args, result},
	{f, arity} = List @@ tok;
	args = Take[stack, -arity];
	stack = Drop[stack, -arity];
	result = Apply[ ToExpression[f, StandardForm, Inactive], args ];
	AppendTo[stack, result];
	AppendTo[history, result];
	If[printDebug, AppendTo[debugHistory,Activate@Map[HoldForm,stack]]];
];

applyTokens[toks_List, args_List] := Block[{stack = args, history = args},
	Map[applyToken, toks];
	stack
];

eval[toks_List, args_List, makeFunction_: True, OptionsPattern[]] := Module[{f},
	f = Activate@Function@Evaluate@Last@applyTokens[toks, args];
	f @@ args
];


(* ::Subsection:: *)
(*Testing...*)



