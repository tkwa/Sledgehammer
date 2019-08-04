(* ::Package:: *)

BeginPackage["Sledgehammer`"];

Begin["`Private`"]


(* ::Subsubsection:: *)
(*Parameters*)


On@Assert;



$base::usage = "Arithmetic encoding base. Should be a power of 2";
$base = 2^45;

(* Tokens that are actually token classes, mapped to their corresponding models *)
$dataEncoders = <|
novelToken -> {encNovelToken, decNovelToken},
intLiteral -> {encIntLiteral, decIntLiteral},
realLiteral -> {encRealLiteral, decRealLiteral},
stringLiteral -> {encStrLiteral, decStrLiteral},
intListLiteral -> {encIntListLiteral, decIntListLiteral}
|>;


(* ::Subsubsection:: *)
(*Shared utilities*)


On@Assert

(* Alter values to have sum range and minimum value 1
Some probability mass is uniformly distributed among tokens.
Approaches optimality with large base.
*)
adjust[probs_Association, range_Integer] := Module[{nprobs},
	If[Length@probs > range, Throw["Too many tokens for range" <> ToString@range]];
	nprobs = probs / Total@probs;
	nprobs*(range - Length@nprobs) + 1 // Round
];

(* Manually set $Context to avoid bug with SequencePredict *)
adjustedProbs[model: _Function | _tokenModel, prevTokens_, range_] := adjust[
	Block[{$Context = "System`"}, model[prevTokens]],
	range];

intervalIndex[x_?IntegerQ, cumProbs_List] := Count[# <= x& /@ cumProbs, True]

(* Returns the new interval resulting from encoding a token *)
updateIntervalE=.
updateIntervalE[model_, prevTokens_, token_, state_Interval] := Module[{range, probs, idx, stepInterval},
	range = Max@state - Min@state + 1; (* avoid invalid interval error on 1 *)
	probs = adjustedProbs[model, prevTokens, range];
	idx = First@FirstPosition[Keys@probs, token, Throw["Token not found!"[token, probs, state]]];
	stepInterval = Floor[Flatten[{0, Most@Accumulate[Values@probs], range}][[{idx, idx + 1}]]] + {0, -1};
	Min@state + stepInterval // Interval
];

updateIntervalD[model_, prevTokens_, x_Integer, state_Interval] := Module[{range, probs, idx, cutoffs, token},
	range = Max@state - Min@state + 1;
	probs = adjustedProbs[model, prevTokens, range];
	cutoffs = Min@state + Floor[ Flatten[{0, Most@Accumulate[Values@probs], range}] ] ; (* floats *)
	idx = intervalIndex[x , cutoffs];
	token = Keys[probs][[idx]];
	{token, Interval[cutoffs[[{idx, idx + 1}]] + {0, -1}]}
];

(* For encoder; operates on values between 0 and base-1 *)
growIntervalE[i_Interval] := Module[{g, f, ret},
	g = 2#+Interval[0,1]&;
	f = Which[
		Max@# < 1/2 base, (Sow@0; g@#),
		Min@# >= 1/2 base, (Sow@1; g@# - base),
		Min@# >= 1/4 base && Max@# < 3/4 base, (Sow[.5]; g@# - 1/2 base),
		True, #
		]&;
	If[Min@i < 0 || Max@i >= base, Throw["Invalid interval"[i, base]]];
	ret = FixedPoint[f, i];
	If[base/4 < Max@ret-Min@ret + 1 <= base, ret, Throw["Invalid interval!"]]
];
(* For decoder *)
growIntervalD[i_Interval, oldX_] := Module[{f, g, x, updateX, ret},
	x=oldX;
	If[Not[0 <= Min@i <= x <= Max@i < base], Throw["Invalid interval"@i]];
	g := (2#+Interval[0,1]&);
	f = Which[
		Max@# < 1/2 base, x = 2x + getBit[]; g@#,
		Min@# >= 1/2 base, x = 2x- base + getBit[]; (g@# - base),
		Min@# >= 1/4 base && Max@# < 3/4 base, x = 2x + - 1/2 base + getBit[]; (g@# - 1/2 base),
		True, #
		] &;
	If[Min@i < 0 || Max@i >= base || x < 0 || x >= base, Throw["Invalid interval"[i, base, x]]];
	ret = FixedPoint[f, i];
	If[base/4 < Max@ret-Min@ret + 1 <= base && Min@ret <= x < Max@ret + 1 , Null, Throw["Invalid interval!"[ret, x]]];
	{ret, x}
];

(* Flushes the encoder to allow bypass *)




(* ::Subsubsection::Closed:: *)
(*Linked list*)


toLinkedList = Fold[{#2, #1}&, linkEnd, Reverse@#]&;
(*reflatten = NestWhile[(Sow@First@#; Last@#)&, #, Length@# \[Equal] 2&]& /* Reap /* (#[[2,1]]&); *)

First[linkEnd] ^= 0;
Last[linkEnd] ^= linkEnd;
linkedListTake[ll: _List | linkEnd, n_Integer] := FoldPairList[
	{First@#, Last@#}&, ll, Range@n
]

linkedListTakeDrop[ll: _List | linkEnd, n_Integer /; n >= 0] := If[n == 0,
	{{}, ll},
	NestList[Last, ll, n-1] // {First/@#, Last@Last@#}&
];
linkedListTakeDrop[args___] := Assert[False, "Argument error in linkedListTakeDrop!"@args]

getBit=.
(* Modifies "global" variable aCoderLinkedList *)
getBit[] := Module[{},
	Assert[! IntegerQ[aCoderLinkedList]];
	linkedListTakeDrop[aCoderLinkedList, 1] // If[IntegerQ@Last@#, Throw[Last@#], #]& // (aCoderLinkedList = Last@#; #[[1,1]])&
]
Clear[getBits];
getBits[0] := {};
getBits[n_Integer /; n > 0] := (Assert[! IntegerQ[aCoderLinkedList]];
linkedListTakeDrop[aCoderLinkedList, n] // If[IntegerQ@Last@#, Throw[Last@#], #]& // (aCoderLinkedList = Last@#; #[[1]])&);
getBits[___] := Assert[False, "Arg error in getBits"];

(* GeneralUtilities`BenchmarkPlot[reflatten @* unflatten, Range, PowerRange[1,10000,2],"IncludeFits" \[Rule] True] *)


(* ::Subsection:: *)
(*Encoding*)


(* Encodes a token from a token model. *)
encodeToken =.
encodeToken[token_, model: _Function | _tokenModel, prevTokens_List: {}] := Module[
{range, probs, keys, stepInterval},
	state = updateIntervalE[model, prevTokens, token // show, state];
	state = growIntervalE[state] // show;
];

encodeToken[a___] := Throw["Type error encoding token: "@a]

(* Returns interval corresponding to next token, and appended token*)
encodeStep=.
encodeStep[token_, model_, prevTokens_List, oldState_Interval] := Block[{head, data, state = oldState},
	If[KeyExistsQ[Head[token]]@$dataEncoders,
		(* encode token with data *)
		head = Head@token;
		encodeToken[head[], model, prevTokens];
		$dataEncoders[head][[1]] @@ token;
		, (* encode token plainly *)
		encodeToken[token, model, prevTokens];
	];
	state
];

rmLiteralRules = {(h:intLiteral | stringLiteral | realLiteral)[_] :> h[]};

pfxpairs=.
(* Creates a list of {prefix, next_element} pairs *)
pfxpairs[l_List] := With[{order = 5}, Thread[{
	Partition[l,order,1,{order+1, order+1},{}] /. rmLiteralRules,
	l
}]];

encodeReap[tokens_List, model_] := Reap@Fold[
	encodeStep[Last@#2, model, First@#2, #]&,
	Interval[{0,base - 1}],
	pfxpairs@tokens];

replaceHalfsF[nHeldBits_, nextBit_] := 
	If[nextBit == .5,
		{{}, nHeldBits + 1},
		{Prepend[ConstantArray[1 - nextBit, nHeldBits], nextBit], 0}
		];
replaceHalfs[l_List] := Catenate@FoldPairList[replaceHalfsF, 0, l];

(* Since the size is at least 1/4, the final interval contains at most 2 bits of information. *)
finalIntervalToBits[i_Interval] := Which[
	Min@i <= base/2 < Max@i + 1, {1},
	Min@i <= base/4 < Max@i + 1, {0,1},
	Min@i <= 3 base/4 < Max@i + 1, {1,1},
	True, Throw["Final interval error"[i, base]]
];

encodeNoPrepend[tokens_List, model_, encodeBase_Integer: $base] := Block[{encoded, base = encodeBase},
	encoded = encodeReap[tokens, model];
	Join[ Catenate@encoded[[2]], finalIntervalToBits[ encoded[[1]] ]] // replaceHalfs // Internal`DeleteTrailingZeros
]

encode[tokens_List, model: _Function | _tokenModel, encodeBase_Integer:$base] :=
    Join[varEliasDelta[Length@tokens, 4, False], encodeNoPrepend[tokens, model, encodeBase]];

SHEncode[tokens_List] := encode[tokens, $tokenModel, $base];

(* ::Subsection:: *)
(*Decoding*)


decodeToken=.
decodeToken[model: _Function | _tokenModel, prevTokens_List: {}] := Module[
{cumProbs, idx, newPrevTokens, newInterval, token},
	{token, state} = updateIntervalD[model, prevTokens, x, state];
	{state, x} = growIntervalD[state, x];
	token
];

(* Decodes one token, taking a rational number; returns token and remaining rational *)
decodeStep[model: _Function | _tokenModel, oldPrevTokens_List, oldState_Interval, oldX_Integer] := Block[
	{prevTokens = oldPrevTokens, token, state = oldState, x = oldX, ret},
	token = decodeToken[model, prevTokens] // show;
	prevTokens = Append[prevTokens, token] // Take[#, -Min[5, Length@prevTokens + 1]]&;
	If[KeyMemberQ[$dataEncoders, Head@token],
		show["Decoding int literal starting with state"[x/base // N]];
		Sow[Head[token][ $dataEncoders[Head@token][[2]][] ]],
		Sow[token]
	];
	{prevTokens, token, state, x}
];

decodeNoPrepend[bits_List, model: _Function | _tokenModel, nToks_Integer, decodeBase_Integer:$base] := (aCoderLinkedList = toLinkedList[bits];
	initialX = FromDigits[Table[getBit[], BitLength@base - 1], 2];
	initialInterval = Interval[{0, base - 1}];
	Reap[Nest[decodeStep[model, #[[1]], #[[3]], #[[4]]]&, {{}, "foo", initialInterval, initialX}, nToks]] // #[[2, 1]]&
);

decode[bits_List, model: _Function | _tokenModel, decodeBase_Integer:$base] := Block[{base = decodeBase, nToks, aCoderLinkedList, initialX, initialInterval},
	aCoderLinkedList = toLinkedList[bits];
	nToks = decVarEliasDelta[4, False, getBits];
	initialX = FromDigits[Table[getBit[], BitLength@base - 1], 2];
	initialInterval = Interval[{0, base - 1}];
	Reap[Nest[decodeStep[model, #[[1]], #[[3]], #[[4]]]&, {{}, "foo", initialInterval, initialX}, nToks]] // #[[2, 1]]&
];

SHDecode[bits_List] := decode[bits, $tokenModel, $base];


(* ::Subsection:: *)
(**)


End[];
EndPackage[];
