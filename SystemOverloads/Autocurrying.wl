(* ::Package:: *)

(* Currying for 2 argument functions *)
Unprotect[Function]
(f_Function /; !FreeQ[f, HoldPattern[#2]])[a_] := f /. {# -> a, #2 -> #};




