(* ::Package:: *)

(* ::Subsection:: *)
(*Imports and setup*)


BeginPackage["SHInterpreter`"];
(* Get path whether run through a notebook or wolframscript -script *)

SetDirectory[DirectoryName[$InputFileName /. "" :> NotebookFileName[]]];

tokToBitsDict = Get["save.mx"] // Map[Rest@IntegerDigits[#,2]&];
bitsToTokDict = AssociationThread[Values@#, Keys@#]&@tokToBitsDict;

On[Assert]
printShows = False;
show := If[TrueQ[printShows], Echo, #&];
(*$IterationLimit = 2^14;*)


(* ::Subsection:: *)
(*Compression*)


show[#, "Token list length = "]& @Length@tokToBitsDict; (* 8324 fixed, 6k more  *)


(* ::Subsubsection:: *)
(*Preprocessing*)


(* Removes all heads that are not symbols or primitives *)
rmCompoundHeads[expr_HoldComplete] := Module[{},
	expr //. {
		p_[f_][x_] :> Construct[p[f], x],
		p_[f_][args___] :> Apply[p[f],{args}]
	}
];

preprocess=.
preprocess[expr_HoldComplete] := Module[{},
	expr /. {Slot[1] -> SHInterpreter`s1, Slot[2] -> SHInterpreter`s2, Slot[3] -> SHInterpreter`s3} // rmCompoundHeads
];

postprocess[expr_HoldComplete] := Module[{},
	expr /. {s1 -> Slot[1], s2 -> Slot[2], s3 -> Slot[3]} /. {Hold[x_Slot] -> x}
];


(* ::Subsubsection:: *)
(*Compressor*)


eliasGamma[0] := {1}
eliasGamma[n_Integer /; n > 0] := IntegerDigits[n,2] // Join[ConstantArray[0,Length@# - 1], #]&;

eliasDelta[n_Integer /; n > 0] := IntegerDigits[n,2] // Join[eliasGamma[Length@#], Rest@# ]&;

eliasDelta[_] := Throw@"Argument to Elias Delta must be a positive integer."

(* Elias delta except for the lower k bits which are explicitly encoded, and the sign which is the first bit if sgnQ = True. *)
varEliasDelta[n_Integer, k_Integer: 3, sgnQ_: True] := Module[{nn, sgn, ret},
	If[!sgnQ, Assert[n >= 0]];
	sgn = Boole[n < 0];
	nn = BitXor[n, -sgn]; (* BitNot[n] if n < 0 else n *)
	ret = Join[eliasDelta[Floor[nn/2^k]+1], IntegerDigits[nn,2,k]];
	If[sgnQ, Join[{sgn},ret], ret]
];

(* modified Elias Delta, mod 2^3 *)
tokenToBits[intLiteral[n_Integer]] := Join[tokenToBits@intLiteral[], varEliasDelta[n, 3, True]];

(* convert real numbers to digit lists *)
tokenToBits[realLiteral[x_Real]] := Module[{str, len},
	str = ToString[x, InputForm];
	len = StringLength@str;
	ToCharacterCode@str // IntegerDigits[#, 2, 7]& // Flatten //
	Join[tokenToBits@realLiteral[],varEliasDelta[len, 3, False],#] &
];

(* ASCII strings packed into 7 bits per character.*)
tokenToBits[asciiLiteral[str_] /; Max@ToCharacterCode@str <= 127] := Module[{len},
	len = StringLength@str;
	ToCharacterCode@str // IntegerDigits[#, 2, 7]& // Flatten //
	Join[tokenToBits@asciiLiteral[],varEliasDelta[len, 3, False],#] &
];

tokenToBits[tok_, encodeDict_: tokToBitsDict] := Lookup[ encodeDict, tok, Assert[False, {"Token not found!",tok}]];

(* remove all extraneous elements from tokens ending on 1s? *)
compress[toks_List] := Join @@ Map[tokenToBits] @ toks /. {a___, 1...} :> {a};
compress[expr_HoldComplete] := compress@wToPostfix@rmCompoundHeads@expr;
compress[str_String] := brailleToBits@str;


(* ::Subsubsection:: *)
(*Lexer/parser*)


ClearAttributes[symbolLiteral, HoldFirst]

postfixtoken::usage = "Converts a Held token to postfix token e.g. Hold[5] -> intLiteral[5]";
postfixtoken[expr_] := Module[{h, name}, Which[
	(* cases: call[], asciiLiteral[], intLiteral[], symbolLiteral[] *)
	Depth@Head@expr == 2, call[SymbolName@@Unevaluated/@Head@expr,Length@expr],
	MatchQ[expr, Hold[_String]], asciiLiteral@@expr,
	MatchQ[expr, Hold[_Integer]], intLiteral@@expr,
	MatchQ[expr, Hold[_Real]], realLiteral@@expr,
	Depth@expr == 2, symbolLiteral[SymbolName@@Unevaluated/@expr],
	True, Assert[False, "Unexpected compound head"]]];

wToPostfix[expr_] := Map[postfixtoken, Level[
		Map[Hold, expr, {-1}, Heads -> True],
		{1,-2}]];

wToPostfix::usage = "Converts WL code HoldComplete[...] to postfix form, fails on compound heads";


(* first item of stack \[Equal] bottom. Return new stack*)
oneTokenToW[stack_, next_] := Module[{isCall, arity, pops, newExpr, newStack, operator},
	(* if an operand, just return *)
	isCall = MatchQ[next, _call];
	Switch[next,
			_intLiteral | _realLiteral | _asciiLiteral | _dictLiteral,
		arity = 0; newExpr = next[[1]];
			_symbolLiteral,
		arity = 0; newExpr = ToExpression[next[[1]], InputForm, HoldComplete];
			_call,
		newExpr = HoldComplete@@newExpr;
		(* if an operator, apply to the last [arity] tokens on the stack *)
		arity = next[[2]];
		pops = Take[stack, -arity];
		newExpr = {pops}; (* List is a dummy head *)
		(* first wrap in HoldComplete *)
		newExpr = HoldComplete@@newExpr;
		(* now delete the inner HoldCompletes *)
		newExpr = DeleteCases[newExpr, HoldComplete, {3}, Heads->True];
		(* now add the operator as the head of newExpr *)
		operator = ToExpression[next[[1]], InputForm, HoldComplete];
		newExpr = newExpr // ReplacePart[{1,0} -> operator] // Delete[{1,0,0}] (* remove the HoldComplete *)
		];
		
	Drop[stack, -arity] // Append[#, newExpr]&
];

postfixToW::usage = "Converts postfix form code to WL code HoldComplete[...]";
postfixToW[pfToks_List] := Module[{},
	Fold[oneTokenToW, {Slot[1]}, pfToks] // Last
];




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
unVarEliasDelta[bits_List, k_Integer:3, sgnQ_:True] := Module[{sgn, rest, ndiv8p1, lenUsed, n},
	sgn = If[sgnQ, First@bits,0];
	rest = If[sgnQ,Rest@bits, bits];
	{ndiv8p1, lenUsed} = unEliasDelta[rest];
	n = (ndiv8p1 - 1) * 2^k + FromDigits[ rest[[lenUsed+1 ;; lenUsed+k]], 2];
	n = BitXor[n,-sgn];
	(* bits used = Elias Delta bits + low bits + sign bit *)
	{n, lenUsed + k + Boole@sgnQ}
];

(* ASCII literal. (Length of string, string in packed 7 bit encoding) *)
decodeASCIILiteral[bits_] := Module[{strLen, lenLen},
	{strLen, lenLen} = unVarEliasDelta[bits, 3, False];
	Drop[bits, lenLen] //
	Take[#, 7 * strLen]& //
	Partition[#,7]& //
	Map[FromDigits[#,2] &] //
	FromCharacterCode //
	{#, lenLen + 7 * strLen} &
];

(* Basic dictionary literal. Index in DictionaryLookup[] in increasing order of length.
	Length <4 words are not useful to store as they're cheaper as regular string literals. *)
decodeDictLiteral[bits_] := Module[{dict, idx, lenUsed},
	dict = Once[DictionaryLookup[] // SortBy[StringLength] // Select[StringLength@# >= 4 &]];
	lenUsed = Once@BitLength@Length@dict;
	idx = Take[bits,lenUsed] // FromDigits[#,2]&;
	{dict[[idx]],lenUsed}
];

(* for future builtins: put n in bijective base k *)
toBijectiveBase[n_Integer, k_Integer] := Module[{acc = n, ret = {}, digit},
	While[acc > 0, 
		digit = Mod[acc-1, k] + 1;
		PrependTo[ret, digit];
		acc = Quotient[acc-1, k]];
	ret
];

(* returns token, length used.*)
bitsToToken[bits_List, decodeDict_: bitsToTokDict] := Module[{pfx, lenUsed, tok},
	(* try increasing prefixes until one is a key of decodeDict *)
	For[lenUsed = 0, lenUsed < 32 && !KeyMemberQ[decodeDict, pfx = Take[bits, lenUsed]], lenUsed++, Null];
	Assert[lenUsed < 32];
	tok = decodeDict[pfx];
	Switch[tok,
		_call , {tok, lenUsed},
		_symbolLiteral , {tok, lenUsed},
		_intLiteral , {intLiteral[#], lenUsed + #2}& @@ unVarEliasDelta[Drop[bits, lenUsed], 3, True],
		_realLiteral, {realLiteral[ToExpression@#], lenUsed + #2}& @@ decodeASCIILiteral[Drop[bits, lenUsed]],
		_asciiLiteral , {asciiLiteral[#], lenUsed + #2}& @@ decodeASCIILiteral[Drop[bits, lenUsed]],
		_dictLiteral, {dictLiteral[#], lenUsed + #2}& @@ decodeDictLiteral[Drop[bits, lenUsed]],
		_, Assert[False, "Token prefix not found!"]
	]
];

(* returns list of tokens *)
decompressNoPad[{}] := {}
decompressNoPad[{Repeated[1]}] := {};

decompressNoPad[bits_List] := Module[{tok, lenUsed},
	{tok, lenUsed} = bitsToToken[bits];
	Prepend[tok][decompressNoPad[Drop[bits, lenUsed]]]];
	
(* decompress after padding with 32 implicit trailing 1 bits *)
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


evalPostfix[toks_List, args_List, makeFunction_: True, OptionsPattern[]] := Module[{f},
	f = Activate@Function@Evaluate@Last@applyTokens[toks, args];
	f @@ args
];

(* Evaluates a function which is wrapped in HoldComplete[] but may be missing Function[]. *)
eval[expr_HoldComplete, args_List, OptionsPattern[]] := Module[{f},
	f = If[expr[[1,0]] === Function, Identity@@expr, Function@@expr];
	f @@ args
];


EndPackage[]
