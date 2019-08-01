(* ::Package:: *)

BeginTestSection["InterpreterTests"]

Begin["Sledgehammer`Private`"];

VerificationTest[(* 1 *)
	Function[List[Slot[1], List[Sledgehammer`unVarEliasDelta[First[Slot[1]], 3, True], Sledgehammer`unVarEliasDelta[Last[Slot[1]], 3, False]]]][List[Sledgehammer`varEliasDelta[5, 3, True], Sledgehammer`varEliasDelta[5, 3, False]]]
	,
	List[List[List[0, 1, 1, 0, 1], List[1, 1, 0, 1]], List[List[5, 5], List[5, 4]]]
	,
	TestID->"unVarEliasDelta"
]

VerificationTest[(* 2 *)
	CompoundExpression[Set[fizzbuzz, HoldComplete[StringRiffle[Array[Function[ReplaceAll[List[StringJoin[Pick[List["Fizz", "Buzz"], Divisible[Slot[1], List[3, 5]]]]], Rule["", Slot[1]]]], 100]]]], Set[pass, Function[Apply[SameQ, List[Slot[1], Sledgehammer`decompress[Sledgehammer`compress[Slot[1]]]]]]], pass[Sledgehammer`wToPostfix[fizzbuzz]]]
	,
	True
	,
	TestID->"Expression compress/Sledgehammer`decompression"
]

VerificationTest[(* 3 *)
	CompoundExpression[Set[fizzbuzz, HoldComplete[StringRiffle[Array[Function[ReplaceAll[List[StringJoin[Pick[List["Fizz", "Buzz"], Divisible[Slot[1], List[3, 5]]]]], Rule["", Slot[1]]]], 100]]]], Set[pass, Function[SameQ[Slot[1], Sledgehammer`postfixToW[Sledgehammer`wToPostfix[Slot[1]]]]]], Map[pass, List[fizzbuzz, Sledgehammer`preprocess[fizzbuzz]]]]
	,
	List[True, True]
	,
	TestID->"Converting between WL form and postfix"
]

VerificationTest[
	expr = HoldComplete[Function[x,MaximalBy[AdjacencyGraph[x,UnitStep[1-DistanceMatrix@x]]~FindShortestPath~##&@@@Tuples[x,2],Length]]];
	expr = expr // RightComposition[
		Sledgehammer`preprocess,
		Sledgehammer`wToPostfix,
		Sledgehammer`compress, 
		Sledgehammer`decompress,
		Sledgehammer`postfixToW,
		Sledgehammer`postprocess] // Echo;
		Sledgehammer`eval[expr, {{"bag", "bat", "cat", "cot", "dot", "dog"}}]
	,
	{{"bag", "bat", "cat", "cot", "dot", "dog"}, {"dog", "dot", "cot", "cat", "bat", "bag"}}
	,
	TestID->"https://codegolf.stackexchange.com/a/187134/39328"
]

End[]
EndTestSection[]


Sledgehammer`compress[{intLiteral[1],symbolLiteral["ss1"], call["Function", 1]}] // Sledgehammer`decompress
