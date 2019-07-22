(* ::Package:: *)

(* credit to https://m athematica.stackexchange.com/a/172718/61597
^ deals with extracting named functions from Mma SE answers; I want to extract Mathematica code blocks.*)

(* << https://paclets.github.io/PacletServer/Install.wl
PublicPacletInstall["ServiceConnection_StackExchange"] *)




(*validSEConn*)
validSEConn[so_]:=
 ServiceConnections`ServiceInformation[so, "Name"]==="StackExchange";

(*getUserID*)
getUserID[se:_ServiceObject?validSEConn,userName_String, site_String: "codegolf"]:=
 se["UserIDs", "site"->site, "inname"->userName][[1]];
  
getAnswers[se:_ServiceObject?validSEConn,ids:{__Integer}] := 
	Apply[Join,
		Map[se["Answers", cgcc, "id" -> StringRiffle[ToString/@#, ";"], "pagesize" -> "100", unsafeBodyFilter]&,
			Partition[ids, UpTo[100]]]
		];

(* $se["Requests"] // Partition[#,4]& // Grid; *)

(* "AnswerQuestion"	"Answers"	"Authentication"	"BasicRequest"
"Comment"	"ID"	"Information"	"LastRequest"
"LinkedQuestions"	"Logout"	"Me"	"Messages"
"MyAnswers"	"MyBadges"	"MyComments"	"MyID"
"MyPosts"	"MyQuestions"	"MyReputation"	"MyReputationHistory"
"MyTags"	"MyTopAnswers"	"MyTopAnswerTags"	"MyTopQuestions"
"MyTopQuestionTags"	"MyTopTags"	"Name"	"NoAnswerQuestions"
"Notifications"	"QueryContinue"	"QueryIterate"	"QuestionAnswers"
"QuestionComments"	"Questions"	"QuestionTimeline"	"QuotaMax"
"QuotaRemaining"	"RawRequests"	"RelatedQuestions"	"RenderQuestions"
"RequestData"	"RequestParameters"	"Search"	"SearchExcerpts"
"SearchSimilar"	"SearchSimple"	"SiteEvents"	"SiteInfo"
"SitePrivileges"	"Sites"	"TagInfo"	"Tags"
"UnansweredQuestions"	"UnreadMessages"	"UnreadNotifications"	"UserAnswers"
"UserBadges"	"UserComments"	"UserConversations"	"UserIDs"
"UserMentions"	"UserPosts"	"UserQuestions"	"UserReputation"
"UserReputationHistory"	"Users"	"UserTags"	"UserTopAnswers"
"UserTopAnswerTags"	"UserTopQuestions"	"UserTopQuestionTags"	"UserTopTags"

 *)
accessToken = "n2SrRsh6m2UPKZMzq*r4Vg))";

query = "Mathematica -Sledgehammer [code-golf] -[restricted-source] -[cops-and-robbers] -[quine] is:a";

cgcc =  "site" -> "codegolf";

unsafeExcerptFilter = "filter" -> "!FcbKgRqyvF.HSpS5cemhVO*6Hu";
unsafeBodyFilter = "filter" -> "7yrx0Ksa";



$se = ServiceConnect["StackExchange" , "New"];


$se["SearchExcerpts", cgcc, "q" -> query, unsafeExcerptFilter]


SetDirectory[DirectoryName[$InputFileName /. "" :> NotebookFileName[]]];
allMmaAns = $se["QueryIterate", "Request" -> "SearchExcerpts", cgcc , "pagesize" -> "100", "MaxIterations" -> "100", "q" -> query, unsafeExcerptFilter];

Put[allMmaAns, "CGCC_Mathematica_Answers.mx"]


SetDirectory[DirectoryName[$InputFileName /. "" :> NotebookFileName[]]];
allMmaAns = Get["CGCC_Mathematica_Answers.mx"];

answerIDs = allMmaAns[All, "answer_id"] // Normal;
answerIDs//Length
Put[answerIDs, "CGCC_Mathematica_Answer_IDs.mx"]


SetDirectory[DirectoryName[$InputFileName /. "" :> NotebookFileName[]]];

answerIDs = Get["CGCC_Mathematica_Answer_IDs.mx"];
(* Non-ASCII characters get replaced with <NotASCII> before this point. Improve corpus by including non-ASCII chars? *)
answers = getAnswers[$se, Take[answerIDs, All]];
Length@answers
(*
getAnswers[ids_List] := Module[{len = Length@ids},
	If[IntegerQ@numAnswers&&numAnswers<100,
		se["Answers", "id" \[Rule] StringRiffle[ids, ";"], cgcc, unsafeBodyFilter, "pagesize"->ToString[numAnswers]],
		se["QueryIterate",
			"Request"->"Answers", If[IntegerQ@numAnswers,
      "MaxIterations"\[Rule]Ceiling[len]
      Sequence@@{}
      ],
     Sequence@@sp
     ]
    ];
 *)
Put[answers, "CGCC_answer_bodies_long.mx"]




Get["C://Users/Thomas/Sledgehammer/SHInterpreter.wl"];
answers = Get["C://Users/Thomas/Documents/Wolfram Mathematica/Sledgehammer Dev/CGCC_answer_bodies_long.mx"];


(* Takes the first string from a <pre><code> block*)
extractCode[block_String]:= Module[{},
   Quiet@
   ToExpression[
    Cases[
		ImportString[block, {"HTML", "XMLObject"}],
		XMLElement["pre", _, {XMLElement["code",_, s_]}] :> StringJoin@DeleteCases[s, Except[_String]],
     Infinity
     ] (* // Sow *),
    InputForm,
    HoldComplete
    ] // DeleteCases[#, $Failed]&
];

titleHasMma = StringContainsQ[StringExtract[#"body", "\n" -> 1], "Mathematica" | "Wolfram"] &;

makeCompoundExpr[expr_HoldComplete] := Module[{},
	If[Length@expr == 1,
		expr,
		HoldComplete[expr] // ReplacePart[{1,0} -> CompoundExpression]
	]
];




SelectFirst[answers, #"answer_id" == 140255&] // #["body"]& // extractCode
SelectFirst[answers, #"answer_id" == 186488&] // #["body"]& // extractCode


getCorpus[as_] := as[All, {"answer_id", "body"}] // 
	Select[titleHasMma] // 
	Normal // 
	Map[#"answer_id" -> #"body"&] // 
	Association //
	Map[extractCode] // 
	DeleteCases[{}] // 
	Map[First] //
	Map[makeCompoundExpr];

corpus = getCorpus[answers];

(*Select[rawstrs, MatchQ[Quiet@ToExpression[#, StandardForm, HoldComplete], $Failed]&]*)
(* 3565 *)
Print[Length@corpus, " answers contain Mma or WL in title"]


Put[corpus, "C:\\Users\\Thomas\\Documents\\Wolfram Mathematica\\Sledgehammer Dev\\training_data.mx"]



Select[corpus, Length@# == 2&] // Take[#, 10]& // Map[makeCompoundExpr] // Normal // Column


answerIDs~Take~10


Head /@ corpus // DeleteDuplicates
