(* ::Package:: *)

Get["Sledgehammer`", Path -> ParentDirectory@ParentDirectory@NotebookDirectory[]];


Once@Get["Sledgehammer`", Path -> ParentDirectory@ParentDirectory@NotebookDirectory[]];

(* Preprocesses the expression, then converts to postfix form *)
makeSeqPrTrainingData[expr_HoldComplete, includeLiterals_:False] := Module[{
	rmLiteralRules = {(h:Sledgehammer`Private`intLiteral | Sledgehammer`Private`stringLiteral | Sledgehammer`Private`realLiteral)[_] -> h[]}},

	expr // preprocess // wToPostfix // If[includeLiterals, #, #  /. rmLiteralRules]&
];
makeSeqPrTrainingData[corpus_Association, includeLiterals_: False] := Module[{},
	makeSeqPrTrainingData[#, includeLiterals]& /@ Values@corpus
];

(* Make a list of stack \[Rule] nextToken test cases *)
makeClassifyTrainingData[expr_HoldComplete] := Module[{},
	preprocess[expr] // 
	Append[
		First@Last@Reap[# // wToPostfix // postfixToW[#, True]&],
		{#} -> "Stop"
	]&
	(* // MapAt[LeafCount, {All, 1}] *)
];

makeClassifyTrainingData[corpus_Association] := Module[{},
	makeClassifyTrainingData /@ Values@corpus // Catenate
];


makeSeqPrTrainingData[Take[myCorpus, 10]]


HoldComplete[Function["abc\(xyz"+1]] /. s_String :> RuleCondition[StringReplace[s, Thread[{"\("} -> {"\\("}]]]


(* ::Subsection:: *)
(*Get corpus (HoldComplete[] expressions) from file*)


myCorpus = Get[Sledgehammer`Private`$PackageDirectory <> "/Development/training_data.mx"];


Off[General::stop]
myCorpus = Get[Sledgehammer`Private`$PackageDirectory <> "/Development/training_data.mx"];
myCorpus = Take[myCorpus, All];
successes[corpus_Association] := Select[corpus, postfixToW@decompress[compress[#]] === preprocess@#&];
Length[myCorpus] - Length[ succs = successes@myCorpus]
(* 1.1992 for 100 answers *)
fails = Complement[myCorpus, succs];
myCorpus = succs;


SHDecode@SHEncode@wToPostfix@preprocess@HoldComplete[PrimeQ /@ Range@5]


decompress[compress[wToPostfix@preprocess@HoldComplete[1+1], Method -> "Arithmetic"], Method -> "Arithmetic"]


myCorpus // Map[ LeafCount@# - 1&] // Histogram[#, Automatic, "CDF"]&


(* ::Subsection:: *)
(*Experiments...*)


ratios = N[compressedLength@#/ 8 / (LeafCount@# - 1)] & /@ succs;
Through@{Median, WinsorizedMean, Mean}@ratios
Histogram[ratios, Automatic, "CDF"]
(* {0.9375`,1.0793182467850493`,1.4650637359936216`} *)


(* ::Subsection:: *)
(*Most and Last Compressible Answers*)


MinimalBy[
	Select[myCorpus, LeafCount@# > 10&],
	N[compressedLength@#/ 8 / (StringLength[ToString[#,InputForm]~StringDelete~Whitespace] - StringLength@"HoldComplete[]")] &, 10] //
 Normal


myCorpus[186488] // compress // decompress // postfixToW
myCorpus[186488] // compressedLength@#/8.&


(* ::Subsection::Closed:: *)
(*Corpus Statistics*)


myCorpus // Map[LeafCount@# - 1&] // Total


Count[myCorpus, List[___Integer], Infinity]


SetAttributes[cnt, HoldFirst]
cnt[pat_] := Count[myCorpus, HoldPattern@pat, Infinity];

cnt[Apply[f_, _, {1}]]
cnt[a: List[__Integer] /; Length@a > 3]


(* ::Subsubsection:: *)
(*Variables*)


Length /@ compress /@ List /@ symbolLiteral /@ {"x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"}


(* ::Subsubsection:: *)
(*Fixed 1st argument*)


Cases[preprocess /@ myCorpus, f_[a: _List | _Integer , args__] :> HoldComplete[f[a]], Infinity] // Counts // TakeLargest[50]


(* ::Subsubsection:: *)
(*Distribution of Strings*)


stringLengths = Cases[myCorpus, _String, Infinity] // Map[N@*StringLength];
Total@stringLengths
Histogram[stringLengths, Automatic, {"Log", "Count"}]


Length @ stringLengths
Count[myCorpus, a_String /; Max@ToCharacterCode@a < 128, Infinity]
Count[myCorpus, a_String /; PrintableASCIIQ[StringDelete[a, "\n" | "\t"]] , Infinity]


(* ::Subsubsection:: *)
(*Distribution of Integers*)


Count[myCorpus, a_Integer /; RealSign@a == #, Infinity]& /@ {1, 0, -1}


Cases[myCorpus, a_Integer /; 1 < Abs@a < 10^10, Infinity] // Abs // Histogram[#, "Log", {"Log", "Count"}]&


Cases[myCorpus, _Rational, Infinity]


Cases[2/3, _Real, Infinity]


(* ::Subsection:: *)
(*Token frequency distribution*)


Take[myCorpus, All] //
makeSeqPrTrainingData //
Map[Subsequences[#, {3}]&]//
Catenate //
Counts //
(Print[TakeLargest[10]@#]; Print[ToString[Count[#, 1]] <> " uniques"]; #)& //
Histogram[#, Automatic, {"Log", "Count"}]&


(* test cell *)
(* Get["C://Users/Thomas/Sledgehammer/SHInterpreter.wl"]; *)



(* ::Subsection:: *)
(*Model*)


SeedRandom[39328];
(* assuming 2 bytes for token for uniques *)
seqprInformation[corpus_] := Module[{ (*train, test, priors, spf *)},
	{train, test} = makeSeqPrTrainingData@corpus// RandomSample // TakeDrop[#, Round[.7*Length@#]]&;
	spf = SequencePredict[train, Method ->  {"Markov", "Order" -> 3}];
	testLen = Total[Length/@test];
	<| "Tokens" -> (spf[test, "SequenceLogProbability"] // Total // # / (-Log[256] testLen)&),
	"Uniques" -> (Complement[Union@@test, Union@@train] // Length // N// #*Log[256, 20000]/testLen &) |>
];

(* Order 3: \[LeftAssociation]"Tokens"\[Rule]0.6546403312427292`,"Uniques"\[Rule]0.014400907283964896`\[RightAssociation]
Order 0: \[LeftAssociation]"Tokens"\[Rule]0.8102177351529412`,"Uniques"\[Rule]0.017032057183103938`\[RightAssociation]
*)

seqprInformation[Take[succs, All]]
Put[spf, Sledgehammer`Private`$PackageDirectory <> "Setup/spf.mx"];


succs[[2]] // wToPostfix // Echo // postfixToW


Begin["Sledgehammer`Private`"]
postfixToW[{intLiteral[5], intLiteral[5], intLiteral[5], call["Plus", 2]}]
End[]


Sledgehammer`Private`spf[{{}}, "Probabilities"] // First // Keys // Length


(* ::Subsubsection:: *)
(*Generating random sequences*)


Begin["Sledgehammer`Private`"];
spf = Get[$PackageDirectory <> "Setup/spf.mx"];
spf[{{}}, "RandomNextElement" -> 20] /.
{intLiteral[] -> intLiteral[1], stringLiteral[] -> stringLiteral[""], realLiteral[] -> realLiteral[.5]} //
First // postfixToW // postprocess // InputForm
End[];


(* ::Subsubsection:: *)
(*Testing spf*)


Begin["Sledgehammer`Private`"]
tokenModel[spf]
End[]


Total[Length/@test]


SeedRandom[39328];
classifierInformation[corpus_] := Module[{train, test, priors, clf},
	{train, test} = makeClassifyTrainingData@corpus// RandomSample // TakeDrop[#, Round[.7*Length@#]]&;
	clf = Classify[train, ClassPriors -> Automatic];
	Information[clf]
];

classifierInformation[Take[myCorpus, 1000]]


{train, test} = RandomSample[makeClassifyTrainingData/@successes] //Map[Append["Stop"]]//TakeDrop[#, Round[.7*Length@#]]&;


anyspf = SequencePredict[train];
entropyDistribution = test // Map[anyspf[#, "SequenceLogProbability"] / (Length@# - 1)&] // #/-Log[256]&;
entropyDistribution // Histogram
Print["Entropy lower bound ", Median@entropyDistribution, " bytes per token (excluding entropy from literals and novel tokens)"]


rmLiteralRules = {(h:intLiteral | stringLiteral | realLiteral)[_] -> h[]};
novelTokens = Complement[Catenate[test /. rmLiteralRules ],Catenate[train /. rmLiteralRules]];
Print[Length@novelTokens, " of ", Length@Catenate@test, " non-literal tokens in test set are novel"]


spf[{{}},"Probabilities"] // First // ReverseSort // Take[#, 20] & // Normal // Column
modelfreqs = spf[{{}},"Probabilities"] // First // Normal // #/. 
	{intLiteral[_] -> intLiteral[], realLiteral[_] -> realLiteral[], stringLiteral[_] -> stringLiteral[]}& //
	Merge[Apply@Plus] // ReverseSort;
Put[modelfreqs, "C:\\Users\\Thomas\\Sledgehammer\\modelfreqs.mx"];


corpus // Take[#,206]& // Map[preprocess] // Map[wToPostfix@#&] // Map[Length]



preprocess@corpus[[206]] // FullForm

ToExpression["9[,6,0,8,2,3,1,7,5,4][[#~Mod~41~Mod~11]]&",TraditionalForm, HoldComplete] // preprocess // compress


trainingSizes = corpus // Map[{LeafCount@preprocess@# - 1, .125*Length@compress@#}&];
Print["Median bytes/token: " trainingSizes // Values // Map[#[[2]]/#[[1]]&] // Median];


SyntaxInformation@Nest
