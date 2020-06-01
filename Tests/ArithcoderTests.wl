(* ::Package:: *)

BeginTestSection["ArithcoderTests"]
Begin["Sledgehammer`Private`"]


(* ::Subsection:: *)
(*Encoder utilities*)


VerificationTest[
	CompoundExpression[
		Set[toyModel, Function[Association[Rule["red", 0.6`], Rule["green", 0.3`], Rule["yellow", 0.1`]]]],
		adjustedProbs[toyModel, List[], 100]]
	,
	Association[Rule["red", 59], Rule["green", 30], Rule["yellow", 11]]
	,
	TestID -> "adjustedProbs"
]

VerificationTest[
	base = 2^8; CompoundExpression[
		Set[toyModel, Function[Association[Rule["red", 0.6`], Rule["green", 0.3`], Rule["yellow", 0.1`]]]],
		Map[Function[updateIntervalE[toyModel, List[], Slot[1], Interval[List[10, 110]]]], List["red", "green", "yellow"]]]
	,
	List[Interval[List[10, 69]], Interval[List[70, 99]], Interval[List[100, 110]]]
	,
	TestID -> "updateIntervalE"
]

VerificationTest[
	CompoundExpression[Set[base, Power[2, 20]], Reap[growIntervalE[Interval[List[500000, 529000]]]]]
	,
	List[Interval[List[135680, 599695]], List[List[0.5`, 0.5`, 0.5`, 0.5`]]]
	,
	TestID -> "growIntervalE"
]



(* ::Subsection:: *)
(*Encoder*)


VerificationTest[
	CompoundExpression[
		Set[toyModel, Function[Association[Rule["red", 0.6`], Rule["green", 0.3`], Rule["yellow", 0.1`]]]],
		Set[base, Power[2, 20]],
		Map[Function[Block[{state = Interval[List[0, Plus[base, -1]]]}, Reap[encodeToken[#, toyModel, List[]]; state ]]], List["red", "green", "yellow"]]]
	,
	{{Interval[{0, 629144}], {}}, {Interval[{209714, 838859}], {{1}}},
     {Interval[{209712, 1048575}], {{1, 1, 1}}}}
     ,
     TestID -> "encodeToken start state"
]

VerificationTest[
	CompoundExpression[
		Set[toyModel, Function[Association[Rule["red", 0.6`], Rule["green", 0.3`], Rule["yellow", 0.1`]]]],
		Set[base, Power[2, 20]],
		Map[Function[Block[{state = Interval[List[0, 234564]]}, Reap[encodeToken[#, toyModel, List[]]; state ]]], List["red", "green", "yellow"]]]
	,
	{{Interval[{0,562951}],{{0,0}}},{Interval[{77328,640287}],{{0,0,1}}},{Interval[{232000,607311}],{{0,0,1,1}}}}
	,
	TestID -> "encodeToken arbitrary state"
]

VerificationTest[
	CompoundExpression[
		Set[aliceModel, Sledgehammer`Private`tokenModel[SequencePredict[List[ToCharacterCode[ExampleData[List["Text", "AliceInWonderland"]]]], Rule["PerformanceGoal", "TrainingSpeed"]], 0]],
		Set[base, Power[2, 20]],
		state = Interval[List[3245, 901234]],
		Reap[encodeToken[First[ToCharacterCode["y"]], aliceModel, ToCharacterCode["golf"]]; state]]
	,
	List[Interval[List[319744, 900991]], List[List[1, 1, 0, 1, 1, 0]]]
	,
	TestID -> "encodeToken aliceModel"
]

VerificationTest[
	Map[pfxpairs, {{}, {1, 2, 3}, {intLiteral[5], call["Map", 2], 3, 1, 4, stringLiteral[""]}}]
	,
	{{},{{{},1},{{1},2},{{1,2},3}},{{{},intLiteral[5]},{{intLiteral[]},call["Map",2]},{{intLiteral[],call["Map",2]},3},{{intLiteral[],call["Map",2],3},1},{{intLiteral[],call["Map",2],3,1},4},{{intLiteral[],call["Map",2],3,1,4},stringLiteral[""]}}}
	,
	TestID -> "pfxpairs"
]

VerificationTest[
	encodeStep["red", toyModel, {}, Interval[{0, base - 1}]]
	,
	Interval[{0,629144}]
	,
	TestID -> "encodeStep"
]

VerificationTest[
	CompoundExpression[
		SeedRandom[314], 
		Set[toyModel, Function[Association[Rule["red", 0.6`], Rule["green", 0.3`], Rule["yellow", 0.1`]]]], 
		Set[toks, RandomChoice[Keys[toyModel[]], 50]], 
		Set[base, Power[2, 20]], 
		encodeReap[toks, toyModel]]
	,
	List[Interval[List[410724, 889111]], List[List[1, 1, 0, 1, 0.5`, 0, 1, 1, 1, 1, 1, 0.5`, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0.5`, 1, 0.5`, 0.5`, 1, 1, 0, 1, 0, 0, 0.5`, 1, 0.5`, 0.5`, 0.5`, 1, 0.5`, 0.5`, 0.5`, 0.5`, 1, 0, 0, 0.5`, 0.5`, 1, 0.5`, 0.5`, 0.5`, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0.5`, 0.5`, 0.5`, 1, 0.5`, 0.5`, 0.5`, 1, 0, 1, 0, 1, 0, 0.5`, 0, 0.5`, 1, 1, 0, 0.5`, 0, 1, 1, 0.5`, 1, 1, 1, 0, 1, 0.5`, 0.5`, 1, 0, 0, 1, 1, 0, 0.5`, 0.5`]]]
	,
	TestID -> "encodeReap"
]

VerificationTest[
	replaceHalfs[List[Times[1, Power[2, -1]], 0, Times[1, Power[2, -1]], 1, 1, Times[1, Power[2, -1]], Times[1, Power[2, -1]], Times[1, Power[2, -1]], 1]]
	,
	List[0, 1, 1, 0, 1, 1, 0, 0, 0]
	,
	TestID -> "replaceHalfs"
]

VerificationTest[
	CompoundExpression[SeedRandom[12345], Set[toyModel, Function[Association[Rule["red", 0.6`], Rule["green", 0.3`], Rule["yellow", 0.1`]]]], Set[toks, RandomChoice[Through[Rule[Values, Keys][toyModel[]]], 50]], Set[base, Power[2, 20]], Function[List[Slot[1], Length[Slot[1]], Times[Length[toks], N[Entropy[2, toks]]]]][encodeNoPrepend[toks, toyModel]]]
	,
	{{0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0,
      0, 0, 1, 1, 1}, 53, 55.12444461577948}
  ,
	TestID -> "Encoding with toy model"
]

VerificationTest[
	CompoundExpression[
		Set[raven, Flatten[ToCharacterCode[StringDelete[ExampleData[List["Text", "TheRaven"]], "\[Copyright]"]]]], 
		AbsoluteTiming[Hash[encodeNoPrepend[Take[raven, 1000], aliceModel]]]]
	,
	{4.38, 4647558289450166322}
	,
	TestID -> "Encode speed test", SameTest->(Last@#1 == Last@#2&), TimeConstraint->10
]



(* ::Subsection:: *)
(*Decoder utilities*)


(* ::Subsubsection:: *)
(*Linked list*)


VerificationTest[
	ll = toLinkedList@Range@3;
	linkedListTake[ll, 5]
	,
	{1,2,3,0,0}
	,
	TestID -> "linkedListTake"
]

VerificationTest[
	aCoderLinkedList = toLinkedList[Range[3]]; {Array[getBit[] & , 5], aCoderLinkedList}
	, 
	{{1,2,3,0,0},linkEnd}
	, 
	TestID -> "getBit"
]

VerificationTest[
	aCoderLinkedList = toLinkedList@Range@3;
	{getBit[], getBit[], getBits[5]}
	,
	{1,2,{3,0,0,0,0}}
	,
	TestID -> "getBits and Reap scoping"
]



(* ::Subsubsection:: *)
(*Decoder state updates*)


VerificationTest[
	toyModel = Association["red" -> 0.6, "green" -> 0.3, "yellow" -> 0.1] & ;
	(intervalIndex[#1, {0, 1, 1, 2, 3, 5, 8}] & ) /@ {1, 2, 3, 4, 5, 6, 7, 999}
    ,
    {3, 4, 5, 5, 6, 6, 6, 7}
    ,
    TestID -> "intervalIndex"
]

VerificationTest[
	base = 2^20; 
	updateIntervalD[aliceModel, {}, 2345542, Interval[{34365, 2349572349}]]
	,
	{32, Interval[{34365, 438187500}]}
	,
	TestID -> "updateIntervalD"
]

VerificationTest[
	charCodes = Flatten[ToCharacterCode["Five boxing wizards jump quickly"]];
	base = 2^45; 
	aCoderLinkedList = toLinkedList[encodeNoPrepend[charCodes, aliceModel]];
	growIntervalD[Interval[{2121372354, 2126675097}], 2123897908]
	, 
	{Interval[{15609391742976, 26730051928063}], 20905862883713}
	,
	TestID -> "encode and growIntervalD"
]

VerificationTest[
	charCodes = Flatten[ToCharacterCode["Five boxing wizards jump quickly"]]; 
	base = 2^45; 
	aCoderLinkedList = toLinkedList[encodeNoPrepend[charCodes, aliceModel]];
	Table[getBit[], 100]
	,
	{0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 
   0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 
   0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1}
	,
	TestID -> "encode and getBit"
]


(* ::Subsection:: *)
(*Decoder*)


VerificationTest[
	charCodes = Flatten[ToCharacterCode["Five boxing wizards jump quickly"]];
	base = 2^45;
	aCoderLinkedList = toLinkedList[encodeNoPrepend[charCodes, aliceModel]]; state = Interval[{0, base - 1}];
	Sledgehammer`Private`x = FromDigits[Table[getBit[], 45], 2];
	{decodeToken[aliceModel, {}], state, Sledgehammer`Private`x}
	,
	{70, Interval[{15609370221568, 26730008906751}], 20905830711458},
   TestID -> "encode and decodeToken"
]

VerificationTest[
	charCodes = Flatten[ToCharacterCode["Programming puzzles and code golf"]];
	base = 2^45; 
	aCoderLinkedList = toLinkedList[encodeNoPrepend[charCodes, aliceModel]];
	decodeStep[aliceModel, {}, Interval[{0, base - 1}], FromDigits[Table[getBit[], 45], 2]]
	, 
	{{80}, 80, Interval[{6446666379264, 25204856195071}], 23273196273538}
	,
	TestID -> "encode and decodeStep"
]

VerificationTest[
	SeedRandom[1234];
	toyModel = Association["red" -> 0.6, "green" -> 0.3, "yellow" -> 0.1] & ;
	a = RandomChoice[Keys@toyModel[], 10] ;
	decodeNoPrepend[encodeNoPrepend[a, toyModel], toyModel, 10]
	,
	{"yellow","red","red","green","yellow","yellow","green","green","yellow","yellow"}
	,
	TestID -> "Enc/dec with toyModel"
]
	

VerificationTest[
	charCodes = Flatten[ToCharacterCode["My hovercraft is full of eels."]];
	b = encodeNoPrepend[charCodes, aliceModel]; 
    FromCharacterCode[decodeNoPrepend[b, aliceModel, Length[charCodes]]]
    , 
    "My hovercraft is full of eels."
    ,
    TimeConstraint -> 10, TestID -> "Encoding and decoding with aliceModel"
]


(* ::Subsection::Closed:: *)
(*Literals*)


VerificationTest[
	base = 2^45;
	Sledgehammer`Private`aCoderLinkedList = toLinkedList@encodeNoPrepend[{1,0,0,1,1,0,1,0,0,1,0}, Sledgehammer`Private`$bitModel];
	Sledgehammer`Private`state = Interval@{0, base-1};
	x = FromDigits[Table[Sledgehammer`Private`getBit[], BitLength@base - 1], 2];
	Sledgehammer`Private`decodeBits[11]
	,
	{1,0,0,1,1,0,1,0,0,1,0}
	,
	TestID -> "Enc/dec raw bits"
]

VerificationTest[
	literalTestModel = <| intLiteral[] -> .5, "foo" -> .5 |>&;
	encodeNoPrepend[{"foo", intLiteral[-5], intLiteral[24601]}, literalTestModel] === Flatten[
	{1, 0, varEliasDelta@-5, 0, varEliasDelta@24601, 1}]
	,
	True
	,
	TestID -> "Encoding int literals", TimeConstraint->1
]

VerificationTest[
	literalTestModel = <| intLiteral[] -> .2, "foo" -> .8 |>&;
	a = Riffle[intLiteral /@ {0, 1, -1, 5, 314, 24601, 1324356}, "foo"];
	decodeNoPrepend[encodeNoPrepend[a, literalTestModel], literalTestModel, 13]
	,
	{intLiteral[0],"foo",intLiteral[1],"foo",intLiteral[-1],"foo",intLiteral[5],"foo",intLiteral[314],"foo",intLiteral[24601],"foo",intLiteral[1324356]}
	,
	TestID -> "Enc/dec integer literals", TimeConstraint->10
	]
	
VerificationTest[
	literalTestModel = <| intLiteral[] -> .2, "foo" -> .5, stringLiteral[] -> .2, realLiteral[] -> .1 |>&;
	a = {"foo", realLiteral[-3.14159]}~Join~Riffle[intLiteral /@ {0, 1, -1, 5, 1324356},
			stringLiteral /@ {" ~!@#$%^&*()", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "~~~~", ""}];
	decodeNoPrepend[encodeNoPrepend[a, literalTestModel], literalTestModel, 10]
	,
	{"foo",realLiteral[-3.14159`],intLiteral[0],stringLiteral[" ~!@#$%^&*()"],intLiteral[1],stringLiteral["ABCDEFGHIJKLMNOPQRSTUVWXYZ"],intLiteral[-1],stringLiteral["~~~~"],intLiteral[5],stringLiteral[""]}
	,
	TestID -> "Enc/dec int/str/real literals", TimeConstraint->10
]

VerificationTest[
	Block[{$names= {"haskell", "julia", "ada"}},
	literalTestModel = <| intLiteral[] -> .2, "foo" -> .4, stringLiteral[] -> .2, realLiteral[] -> .1, novelToken[] -> .1 |>&;
	a = {"foo", realLiteral[-3.14159], novelToken[call["haskell", 5]] }~Join~Riffle[intLiteral /@ {0, 1, -1, 5, 1324356},
			stringLiteral /@ {" ~!@#$%^&*()", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "~~~~", ""}];
	decodeNoPrepend[encodeNoPrepend[a, literalTestModel], literalTestModel, 10]]
	,
	{"foo",realLiteral[-3.14159`],novelToken[call["haskell",5]],intLiteral[0],stringLiteral[" ~!@#$%^&*()"],intLiteral[1],stringLiteral["ABCDEFGHIJKLMNOPQRSTUVWXYZ"],intLiteral[-1],stringLiteral["~~~~"],intLiteral[5]}	,
	TestID -> "Enc/dec int/str/real/novel token literals", TimeConstraint->10
]


(* ::Subsection:: *)
(*All*)


VerificationTest[
	spftm = tokenModel[$spf];
	toks = {symbolLiteral["PrimeQ"], intLiteral[5], call["Range", 1], call["Map", 2]};
	decode[encode[toks, spftm], spftm]
	,
	{symbolLiteral["PrimeQ"],intLiteral[5],call["Range",1],call["Map",2]}
	,
	TestID->"Compression with length prepend"
]


VerificationTest[
	expr = {Combinatorica`FirstLexicographicTableau};
	expr
	,
	{Combinatorica`FirstLexicographicTableau}
	,
	TestID->"TODO: Write novel token test"
]




End[]
EndTestSection[]
