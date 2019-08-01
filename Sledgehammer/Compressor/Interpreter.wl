(* ::Package:: *)

(* ::Subsection:: *)
(*Imports and setup*)


BeginPackage["Sledgehammer`"];

(* Get path whether run through a notebook or wolframscript -script *)
(* SetDirectory[DirectoryName[$InputFileName /. "" :> NotebookFileName[]]]; *)
Begin["`Private`"];
tokToBitsDict = Get["compression_dict.mx", Path -> $PackageDirectory <> "Setup/"] // Map[Rest@IntegerDigits[#,2]&];
bitsToTokDict = AssociationThread[Values@#, Keys@#]&@tokToBitsDict;
toks = Keys[tokToBitsDict];

On@Assert
printShows = False;
show := If[TrueQ[printShows], Echo, #&];
(*$IterationLimit = 2^14;*)


(* ::Subsection:: *)
(*Compression*)


show[#, "Token list length = "]& @Length@tokToBitsDict; (* 8324 fixed, 6k more  *)


(* ::Subsubsection::Closed:: *)
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
freeVars[expr_HoldComplete] := Module[{contexts = {"System`", "Combinatorica`", "Sledgehammer`"}},
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

(* Often in PPCG, there is a top-level CompoundExpression with all but the last being assigning system names to variables.
This should be removed; in future versions adaptive compression should golf code with repeated commands. *)
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
		pops = PadLeft[stack, arity, s1];
		(* replace head with HoldComplete *)
		newExpr = HoldComplete@@{pops};
		(* delete the inner HoldCompletes *)
		newExpr = DeleteCases[newExpr, HoldComplete, {3}, Heads->True];
		(* add the operator as the head of newExpr *)
		operator = ToExpression[next[[1]], InputForm, HoldComplete];
		newExpr = newExpr // ReplacePart[{1,0} -> operator] // Delete[{1,0,0}] (* remove the HoldComplete on the head *)
		];

	PadLeft[stack, Max[Length@stack, arity], s1] // Drop[#, -arity]& // Append[#, newExpr]&
];

postfixToW::usage = "Converts postfix form code to WL code HoldComplete[...].";
postfixToW[pfToks_List, sow_:False] := Block[{f, $Context = "Sledgehammer`Private`"},
	f = If[sow, oneTokenToW@@Sow[Rule@##]&, oneTokenToW];
	Fold[f, {}, pfToks] //
	If[Length@# == 1,
		Last@#,
		Delete[{1,#,0}& /@ Range@Length@#][HoldComplete@#]]&  (* turn multiple to list *)
];


(* ::Subsubsection:: *)
(*Novel token marking*)


$spfStaticProbs = First@Sledgehammer`Private`$spf[{{}},"Probabilities"];
markNovelTokens[toks_List] := toks /. (tok_ /; Not@KeyMemberQ[$spfStaticProbs, tok] :> novelToken[tok])

unmarkNovelTokens[toks_List] := toks /. novelToken[a_] :> a;


(* ::Subsubsection::Closed:: *)
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


(* ::Subsubsection::Closed:: *)
(*Decompressor*)


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


(* Evaluates a function which is wrapped in HoldComplete[] but may be missing Function[]. 
Captures definition with a single Downvalue, but leaks symbol.
TODO: Move to postfixToW
*)
eval[expr_HoldComplete, args_List, OptionsPattern[]] := Module[{f},
	f = Which[
			(* Function *)
			expr[[1,0]] === Function, Identity@@expr,
			(* single DownValue *)
			MatchQ[expr, HoldComplete[a_Symbol[___] := _]], (Identity@@expr; expr[[1, 1, 0]]),
			True, Identity@@expr];
	f @@ args
];


(* ::Subsection:: *)
(*Interactive app*)

formatRPRules = {intLiteral[i_] :> i, realLiteral[r_] :> r, asciiLiteral[s_] :> s,
	call[c___] :> "call"[c], symbolLiteral[s___] :> "sym"[s]};

SledgehammerGUI[] := DynamicModule[{boxContents = "", expr, input = {}, code, pfCode, cmpCode, brCode, lenBits, lenBytes, decompCode, cbs, result, cgccString},
	expr[b_]:=preprocess[MakeExpression[StripBoxes@b,StandardForm]];
	pfCode[] := wToPostfix@expr@code;
	cmpCode[] := compress@wToPostfix@expr@code;
	brCode[] := bitsToBraille@cmpCode[];
	lenBits[] := Length@cmpCode[];
	lenBytes[] := .125 * lenBits[];
	decompCode[] := brCode[] // brailleToBits // decompress // postfixToW // postprocess;
	result[] := TimeConstrained[eval[decompCode[], input], 10];
	cgccString[] := ("# [Sledgehammer](https://github.com/tkwa/Sledgehammer), " <>
			ToString@Ceiling@lenBytes[] <>
			" bytes\n\n    " <>
			brCode[] <>
			"\n\nDecompresses into this Wolfram Language function: \n\n    "<>
			ToString[First@decompCode[], InputForm] <> "\n");

	Deploy@Panel@Column[{"Put code below:",
		EventHandler[
			InputField[ Dynamic@boxContents,Boxes,FieldSize->{{40,80},{5,\[Infinity]}},BaseStyle->{"Notebook", "Input",InputFieldBoxOptions->{"ReturnEntersInput"->False}, ShowCodeAssist ->  True, ShowSyntaxStyles -> True}],{"ReturnKeyDown" :> Paste["\n"],  {"MenuCommand", "HandleShiftReturn"} :> Paste["\n"]}
		],
		Style[Row@{Dynamic@Length@pfCode[]," tokens"},FontSize->Larger],
		Style[Row@{Dynamic@lenBits[]," bits = ",Dynamic@lenBytes[] , " bytes"},FontSize->Larger],
		Row[{"Input (as a list): ",InputField[Dynamic@input]}],
		Button["Update", code = boxContents],

		CheckboxBar[Dynamic@cbs,{"Show postfix","Show bits","Show Braille","Decompress", "Eval"}],
		Dynamic@If[MemberQ[cbs,"Show postfix"],pfCode[] /. formatRPRules, ""],
		Dynamic@If[MemberQ[cbs,"Show bits"],cmpCode[],""],
		Dynamic@If[MemberQ[cbs,"Show Braille"],brCode[],""],
		Dynamic@If[MemberQ[cbs,"Decompress"],decompCode[] // InputForm,""],
		Dynamic@If[MemberQ[cbs,"Eval"],Dynamic@result[],""],

		OpenerView@{"CGCC submission",Column[{Button["Copy",CopyToClipboard[cgccString[]]],
				Framed[Dynamic@Style[cgccString[], FontFamily-> "Courier"]]}
			]}
	}]
	,
	UnsavedVariables -> {}
];


End[];
EndPackage[];
