(* ::Package:: *)

f1 = (# + #2) / (# - #2)&

(* Currying for 2 argument functions *)
Unprotect[Function]
(f_Function /; !FreeQ[f, HoldPattern[#2]])[a_] := f /. {# -> a, #2 -> #};

f1[5]
f1[3,4]






Take[Range@Infinity, 10]



