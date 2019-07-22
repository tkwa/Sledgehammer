(* ::Package:: *)

(* Load all package fragments following https://mathematica.stackexchange.com/a/176489/61597 *)
Sledgehammer`Private`$PackageDirectory = DirectoryName[$InputFileName]
PrependTo[$Path, Sledgehammer`Private`$PackageDirectory];

<< "Declarations.wl"

<< "Functions/Arrays.wl"
<< "Functions/Automata.wl"
<< "Functions/Constants.wl"
<< "Functions/Entities.wl"
<< "Functions/ExternalLangs.wl"
<< "Functions/Functional.wl"
<< "Functions/General.wl"
<< "Functions/Lists.wl"
<< "Functions/Math.wl"
<< "Functions/Sets.wl"

<< "Unprotects.wl"
(*
<< "Overloads/Currying.wl"
<< "Overloads/Operators.wl"
<< "Overloads/String.wl"
*)

(*
<< "Arithcoder.wl"
*)
<< "Interpreter.wl"
