(* ::Package:: *)

(* ::Subsection:: *)
(*Imports and setup*)


BeginPackage["SHInterpreter`"];
$RecursionLimit = 4096;

(* Get path whether run through a notebook or wolframscript -script *)
SetDirectory[DirectoryName[$InputFileName /. "" :> NotebookFileName[]]];
Needs["SHUtils`"]

tokToBitsDict;
bitsToTokDict;
preprocess;
postprocess;
wToPostfix;
postfixToW;
compress;
decompress;
tokenToBits;
bitsToToken;
eval;

(* Begin["Private`"]; *)
tokToBitsDict = Get["compression_dict.mx"] // Map[Rest@IntegerDigits[#,2]&];
bitsToTokDict = AssociationThread[Values@#, Keys@#]&@tokToBitsDict;
toks = Keys[tokToBitsDict];

On[Assert]
printShows = False;
show := If[TrueQ[printShows], Echo, #&];
(*$IterationLimit = 2^14;*)


(* ::Subsection:: *)
(*Literal encodings*)


(* ::Subsubsection:: *)
(*Integers (Elias gamma, delta)*)


eliasGamma[0] := {1}
eliasGamma[n_Integer /; n > 0] := IntegerDigits[n,2] // Join[ConstantArray[0,Length@# - 1], #]&;

eliasDelta[n_Integer /; n > 0] := IntegerDigits[n,2] // Join[eliasGamma[Length@#], Rest@# ]&;

eliasDelta[_] := Throw@"Argument to Elias Delta must be a positive integer."

(* Elias delta except for the lower k bits which are explicitly encoded, and the sign which is the first bit if sgnQ = True. *)
varEliasDelta[n_Integer, k_Integer: 1, sgnQ_: True] := Module[{nn, sgn, ret},
	If[!sgnQ, Assert[n >= 0]];
	sgn = Boole[n < 0];
	nn = BitXor[n, -sgn]; (* BitNot[n] if n < 0 else n *)
	ret = Join[eliasDelta[Floor[nn/2^k]+1], IntegerDigits[nn,2,k]];
	If[sgnQ, Join[{sgn},ret], ret]
];

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

(* returns integer, length used
To do: Change integer literals to k=1 *)
unVarEliasDelta::usage = "Decode a variant Elias Delta bitstring";
unVarEliasDelta[bits_List, k_Integer:1, sgnQ_:True] := Module[{sgn, rest, ndiv8p1, lenUsed, n},
	sgn = If[sgnQ, First@bits,0];
	rest = If[sgnQ,Rest@bits, bits];
	{ndiv8p1, lenUsed} = unEliasDelta[rest];
	n = (ndiv8p1 - 1) * 2^k + FromDigits[ rest[[lenUsed+1 ;; lenUsed+k]], 2];
	n = BitXor[n,-sgn];
	(* bits used = Elias Delta bits + low bits + sign bit *)
	{n, lenUsed + k + Boole@sgnQ}
];

(* modified Elias Delta, mod 2^3 *)
tokenToBits[intLiteral[n_Integer]] := Join[tokenToBits@intLiteral[], varEliasDelta[n, 1, True]];


(* ::Subsubsection:: *)
(*Reals*)


(* convert real numbers to digit lists *)
tokenToBits[realLiteral[x_Real]] := Module[{str, len, bits},
	str = ToString[x, InputForm];
	bits = fromBijectiveBase[(ToCharacterCode@str /. 10 -> 127) - 31, 96]~IntegerDigits~2;
	len = Length@bits;
	Join[tokenToBits@realLiteral[], varEliasDelta[len, 3, False], bits]
];



(* ::Subsubsection::Closed:: *)
(*Bijective base*)


(* convert n to bijective base k *)
toBijectiveBase[n_Integer, k_Integer] := Module[{acc = n, ret = {}, digit},
	While[acc > 0,
		digit = Mod[acc-1, k] + 1;
		PrependTo[ret, digit];
		acc = Quotient[acc-1, k]];
	ret
];

fromBijectiveBase[l_List, k_Integer] := FromDigits[l, k];


(* ::Subsubsection:: *)
(*Strings*)


(* ASCII strings packed into 7 bits per character.*)
tokenToBits[asciiLiteral[str_String] /; SubsetQ[Union[{10}, Range[32, 127]], Union@ToCharacterCode@str] ] := encodeAsciiLiteral[str];
tokenToBits[asciiLiteral[str_]] := tokenToBits[asciiLiteral[""]];

encodeAsciiLiteral[str_String] := Module[{len, bits},
	bits = fromBijectiveBase[(ToCharacterCode@str /. 10 -> 127) - 31, 96]// IntegerDigits[#,2]&;
	len = Length@bits;
	Join[tokenToBits@asciiLiteral[], varEliasDelta[len, 3, False], bits]
];

(* ASCII literal. (Length of string, string in packed 7 bit encoding) *)
decodeAsciiLiteral[bits_] := Module[{len, lenLen},
	{len, lenLen} = unVarEliasDelta[bits, 3, False];
	Drop[bits, lenLen] //
	Take[#, len]& //
	toBijectiveBase[FromDigits[#, 2], 96]& //
	(# + 31 /. 127 -> 10&) //
	FromCharacterCode //
	{#, lenLen + len } &
];


HoldComplete[StringCases[#1,RegularExpression["[A-Za-z](?![A-Za-z])"]]<>""&] // preprocess // FullForm


(* ::Subsection:: *)
(*Compression*)


show[#, "Token list length = "]& @Length@tokToBitsDict; (* 8324 fixed, 6k more  *)


(* ::Subsubsection:: *)
(*Preprocessing*)


(* ASCII source can contain non-ASCII strings; avert this by undoing escapes. *)
fixStrings[expr_HoldComplete] := Module[{},
	expr /. s_String :> RuleCondition[
		StringTake[ToString[s, InputForm, CharacterEncoding -> "PrintableASCII"], {2, -2}] //
       StringReplace[#, "\\n" -> "\n"]&]
];

undoIdioms[expr_HoldComplete] := Module[{},
	expr /. {
		HoldPattern[#&@@x_] :> First@x,
		HoldPattern[x_[[1]]] :> First@x,
		HoldPattern[StringJoin[args__, ""]] :> StringJoin[args],
		HoldPattern[StringJoin["", args__]] :> StringJoin[args] }
];

(* Removes all heads that are not atomic. *)
rmCompoundHeads[expr_HoldComplete] := Module[{},
	expr //. {
		 p_[f___][x_] :> Construct[p[f], x],
		 p_[f___][args___] :> Apply[p[f],{args}]
		}
];

(* Restructures illegal argument patterns and certain special cases of non-function heads.
   To do: replace user-defined variable heads with Construct or Apply *)
fixIllegalCalls[expr_HoldComplete] := Module[{},
	expr /.
		{  f_Symbol[ss_SlotSequence] /;
			MemberQ[toks, symbolLiteral[SymbolName@f]] &&
			Not@MemberQ[toks, call[SymbolName@f, 1]] :> Apply[f, {ss}] ,
		(imm:_Integer|_String)[args___] :> Apply[imm, {args}] }
];
rmDeprecatedTokens[expr_HoldComplete] := Module[{},
	expr /.
		{HoldPattern[Random[]] :> RandomReal[],
		HoldPattern@Date[] :> DateList[]}
];
(* Returns free variables sorted by first order of appearance. *)
freeVars[expr_HoldComplete] := Module[{contexts = {"System`", "Combinatorica`", "SHUtils`"}},
    Cases[expr, s_Symbol /; Not@MemberQ[contexts, Context@s] -> HoldPattern[s], {-1}, Heads-> True] //
    Counts // ReverseSort // Keys (* DeleteDuplicates*)
];
(* May fail on expressions that already contain HoldComplete[_Symbol].
	May fail on expressions that rely on manipulating symbol names.
	May fail on expressions that contain more than 16 free variables. *)
renameFreeVars[expr_HoldComplete] := Module[{vars = freeVars@expr, newvars},
	newvars = ToExpression[#, StandardForm, HoldComplete]&@Array["x" <> ToString@#&, Length@vars];
	Replace[expr /. Thread[vars -> newvars], HoldComplete[s_Symbol] -> s, {2, Infinity}, Heads->True]
];
renameSlotVars[expr_HoldComplete] := Module[{},
	expr /. {Slot[1] -> s1,
		Slot[2] -> s2,
		Slot[3] -> s3,
		SlotSequence[1] -> ss1}
];
(* Often in PPCG, there is a top-level CompoundExpression with all but the last being assigning system names to variables. *)
undoTokenAliases[expr_HoldComplete] := Module[{},
	FixedPoint[
		Replace[
			{HoldComplete@CompoundExpression[Set[var_Symbol, symb_Symbol], rest___] :> (HoldComplete@CompoundExpression@rest /. var -> symb),
			HoldComplete@Function@CompoundExpression[Set[var_Symbol, symb_Symbol], rest___] :> (HoldComplete@Function@CompoundExpression@rest /. var -> symb)
			}],
		expr] /.
	HoldComplete[CompoundExpression[e_]] :> HoldComplete[e]
];
(* Required order of preprocessing steps:
undoTokenAliases \[Rule] renameFreeVars (so free vars are contiguous x1-xn)
*)
preprocess=.
preprocess[expr_HoldComplete] :=
	expr // RightComposition[
	fixStrings,
	rmDeprecatedTokens,
	undoIdioms,
	rmCompoundHeads,
	fixIllegalCalls,
	renameFreeVars,
	renameSlotVars,
	undoTokenAliases];


(* ::Subsubsection:: *)
(*Postprocessing*)


(* undo renameSlotVars *)
restoreSlotVars[expr_HoldComplete] := Module[{},
	expr /. {s1 -> Slot[1], s2 -> Slot[2], s3 -> Slot[3], ss1 -> SlotSequence[1]} /. {Hold[x_Slot] -> x}
];
restoreStrings[expr_HoldComplete] := Module[{},
	expr /. s_String :> RuleCondition[
		ToExpression["\"" <> StringReplace[s, "\n" -> "\\n"] <> "\"", InputForm]]
];

postprocess[expr_HoldComplete] := expr // Composition[
	restoreStrings,
	restoreSlotVars]


(* ::Subsubsection:: *)
(*Compressor*)


tokenToBits[tok_, encodeDict_: tokToBitsDict] := Lookup[ encodeDict, tok, Assert[False, {"Token not found: " tok}]];

(* remove all extraneous elements from tokens ending on 1s? *)
compress=.
compress[toks_List] := Join @@ Map[tokenToBits] @ toks /. {a___, 1...} :> {a};
compress[expr_HoldComplete] := Check[compress@wToPostfix@preprocess@expr,
Assert[False, "Could not compress expression"@expr]];

compressedLength=.
compressedLength[expr: _HoldComplete | _List] := Length @ compress @ expr;

compress[str_String] := brailleToBits@str;


(* ::Subsubsection:: *)
(*Parser (converts between WL expressions and list of postfix tokens)*)


ClearAttributes[symbolLiteral, HoldFirst]

postfixtoken::usage = "Converts a Held token to postfix token e.g. Hold[5] -> intLiteral[5]";
postfixtoken[expr_] := Module[{h, name}, Which[
	(* cases: call[], asciiLiteral[], intLiteral[], symbolLiteral[] *)
	MatchQ[Head@expr, Hold[_Symbol]], call[SymbolName@@Unevaluated/@Head@expr,Length@expr],
	MatchQ[expr, Hold[_String]], asciiLiteral@@expr,
	MatchQ[expr, Hold[_Integer]], intLiteral@@expr,
	MatchQ[expr, Hold[_Real]], realLiteral@@expr,
	Depth@expr == 2, symbolLiteral[SymbolName@@Unevaluated/@expr],
	Depth@expr == 1, Throw[{"Unexpected token", expr}],
	True, Throw["Unexpected compound head", expr]]];

(* Uses WL's expression structure to get tokens in order of evaluation, then converts each to postfix. *)
wToPostfix[expr_] := Map[postfixtoken, Level[
		Map[Hold, expr, {-1}, Heads -> True],
		{1,-2}]];

wToPostfix::usage = "Converts WL code HoldComplete[...] to postfix form, fails on compound heads";


(* ::Subsubsection:: *)
(*Unparser (converts postfix tokens back to WL expressions)*)


(* Takes the current stack and the next postfix token, and returns the new stack.
first item of stack \[Equal] bottom.
This is O(len * stack_depth). Fix? *)
oneTokenToW[stack_, next_] := Module[{isCall, arity, pops, newExpr, newStack, operator},
	(* if an operand, just return *)
	isCall = MatchQ[next, _call];
	Switch[next,
			_intLiteral | _realLiteral | _asciiLiteral | _dictLiteral,
		arity = 0; newExpr = HoldComplete @@ {next[[1]]};
			_symbolLiteral,
		arity = 0; newExpr = ToExpression[next[[1]], InputForm, HoldComplete];
			_call,
		newExpr = HoldComplete@@newExpr;
		(* if an operator, apply to the last [arity] tokens on the stack *)
		arity = next[[2]];
		pops = Take[stack, -arity];
		(* replace head with HoldComplete *)
		newExpr = HoldComplete@@{pops};
		(* delete the inner HoldCompletes *)
		newExpr = DeleteCases[newExpr, HoldComplete, {3}, Heads->True];
		(* add the operator as the head of newExpr *)
		operator = ToExpression[next[[1]], InputForm, HoldComplete];
		newExpr = newExpr // ReplacePart[{1,0} -> operator] // Delete[{1,0,0}] (* remove the HoldComplete on the head *)
		];

	Drop[stack, -arity] // Append[#, newExpr]&
];

postfixToW::usage = "Converts postfix form code to WL code HoldComplete[...]. Throws an error when more than one item is left on the stack.";
postfixToW[pfToks_List, sow_:False] := Module[{f},
	f = If[sow, oneTokenToW@@Sow[Rule@##]&, oneTokenToW];
	Fold[f, {}, pfToks] // If[Length@# == 1, Last@#, Throw[{">1 expression left on stack", pfToks}]]&
];


(* ::Subsection:: *)
(*Decompression*)


(* Basic dictionary literal. Index in DictionaryLookup[] in increasing order of length.
	Length <4 words are not useful to store as they're cheaper as regular string literals. *)
decodeDictLiteral[bits_] := Module[{dict, idx, lenUsed},
	dict = Once[DictionaryLookup[] // SortBy[StringLength] // Select[StringLength@# >= 4 &]];
	lenUsed = Once@BitLength@Length@dict;
	idx = Take[bits,lenUsed] // FromDigits[#,2]&;
	{dict[[idx]],lenUsed}
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
		_intLiteral , {intLiteral[#], lenUsed + #2}& @@ unVarEliasDelta[Drop[bits, lenUsed], 1, True],
		_realLiteral, {realLiteral[ToExpression@#], lenUsed + #2}& @@ decodeAsciiLiteral[Drop[bits, lenUsed]],
		_asciiLiteral , {asciiLiteral[#], lenUsed + #2}& @@ decodeAsciiLiteral[Drop[bits, lenUsed]],
		_dictLiteral, {dictLiteral[#], lenUsed + #2}& @@ decodeDictLiteral[Drop[bits, lenUsed]],
		_, Throw["Token prefix not found!"@tok]
	]
];

(* returns list of tokens *)
decompressNoPad[{}] := {}
decompressNoPad[{Repeated[1]}] := {};

(* Currently O(length^2)! Fix! *)
decompressNoPad[bits_List] := Module[{tok, lenUsed},
	{tok, lenUsed} = bitsToToken[bits];
	Prepend[tok][decompressNoPad[Drop[bits, lenUsed]]]];

(* decompress after padding with 128 implicit trailing 1 bits *)
decompress[bits_List] := decompressNoPad[ArrayPad[bits, {0,128},1]];


(* ::Subsection::Closed:: *)
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


(* This cell is redundant now that postfixToW exists.
add more types of literals later *)

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


(* End[] *)
EndPackage[]
