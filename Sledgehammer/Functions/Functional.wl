(* ::Package:: *)

BeginPackage["Sledgehammer`"];

Begin["Private`"];


(* ::Subsection:: *)
(*Identities*)


left[a_, args___] := a;
right[args___, w_] := w;


(* ::Subsection:: *)
(*Tacit functions*)


(*
Does not enter heads tacitVerbatim and Compose.
Source: https://mathematica.stackexchange.com/a/201075/61597
*)
SetAttributes[{Fork1, hide}, HoldFirst]
Fork1[e_]:=With[{g = hide[e]},
    Replace[
        g[[1]],
        s_Symbol /; !MemberQ[Attributes[s], Temporary] :> s[#],
        {2, Infinity}
    ] /. g[[2]]
]
hide[e_] := Reap[
    ReplaceAll[
        Function[e],
        {SHUtils`tacitVerbatim [t_] :> RuleCondition @ Module[{x}, Sow[x, Unevaluated[t]]],
         c_Compose :> RuleCondition@Module[{x}, Sow[x, Unevaluated@c]]
         }
    ],
    _,
    toRuleDelayed
]
SetAttributes[toRuleDelayed,HoldAll]

toRuleDelayed[x_, {s_}] := s :> x

Fork1[expr_, arg_] := Fork1[expr]@arg;


(* ::Subsection:: *)
(*Functional Iteration and Inverses*)


Under[f_, g_] := InverseFunction[g]@*f@*g


(* ::Subsection:: *)
(*Sequences*)


(* Smallest positive integer where property f is True (todo: or nonzero) 
todo: add options*)
SelectInteger[f_] := NestWhile[#+1&, 1, Not@*f]
SelectInteger[f_, k_Integer] := NestWhile[#+1&, k+1, Not@*f]

(* Smallest n positive integers where property f is True *)
SelectIntegers[f_, n_Integer, k_Integer: 1] := NestList[nFindFrom[f, #]&, k, n-1]


(* ::Subsection:: *)
(**)


End[];
EndPackage[]
