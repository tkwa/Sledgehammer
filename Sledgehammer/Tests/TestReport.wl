(* ::Package:: *)

(* 
Testing framework source:
https://mathematica.stackexchange.com/a/176573/61597
*)

Get["Sledgehammer`", Path -> FileNameJoin[{NotebookDirectory[], "..", ".."}] ]
Begin["Sledgehammer`Private`"];


(* ::Subsubsection::Closed:: *)
(*printTestResults*)


getTestResults[tr_TestReportObject]:=Module[
  {fields,results,abbreviations},

   (* Add other querries to this list. *)
  fields={"TestIndex","Outcome","AbsoluteTimeUsed","MemoryUsed","TestID"};
  abbreviations={"TestIndex"->"Idx","AbsoluteTimeUsed"->"Time [s]"};

  results=ReplaceRepeated[
    Outer[#1[#2]&,Values[tr["TestResults"]],fields],
    {x_Quantity:>QuantityMagnitude[x],x_Real:>Round[x,0.001]}
  ];

  Join[{fields/.abbreviations},results]
]

printTestResults[tr_TestReportObject]:=Module[
  {list,indx,time,noTests},

  list=getTestResults[tr];
  indx=MapIndexed[
    If[
        MemberQ[#1,"Failure"|"MessagesFailure"|"Error"],
        First[#2],
        Nothing
    ]&,
    list
  ];
  time=Round[QuantityMagnitude[tr["TimeElapsed"]],0.01];
  noTests=Length[tr["TestResults"]];

  Print[noTests," tests run in ",time," seconds."];

  If[
    TrueQ@tr["AllTestsSucceeded"],
    Print["All tests succeeded!"],
    Print[tr["TestsFailedCount"]," tests failed!"];
  ];

  Print@Grid[list,
    Alignment->Left,
    Dividers->{None,{2->True}},
    Background->{None,Thread[indx->LightPink]}
  ];
  tr
]


(* ::Subsection:: *)
(*Test reports*)


aCoderLinkedList


$ArithcoderTestReport = TestReport @ FileNameJoin[{NotebookDirectory[],"ArithcoderTests.wl"}];

printTestResults[$ArithcoderTestReport]
$ArithcoderTestReport["TestsFailed"]



$InterpreterTestReport = TestReport @ FileNameJoin[{NotebookDirectory[],"InterpreterTests.wl"}];

printTestResults[$InterpreterTestReport];
$InterpreterTestReport["TestsFailed"]


End[]
Quit[]
