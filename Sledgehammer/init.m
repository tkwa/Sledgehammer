(* ::Package:: *)

$RecursionLimit = 4096;


(* Load all package fragments following https://mathematica.stackexchange.com/a/176489/61597 *)
Sledgehammer`Private`$PackageDirectory = DirectoryName[$InputFileName]
PrependTo[$Path, Sledgehammer`Private`$PackageDirectory];

Sledgehammer`Private`$spf = Get[FileNameJoin@{Sledgehammer`Private`$PackageDirectory, "Setup", "spf.mx"}]




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

<< "SystemOverloads/Unprotects.wl"
(*
<< "SystemOverloads/Currying.wl"
<< "SystemOverloads/Operators.wl"
<< "SystemOverloads/String.wl"
*)

<< "Compressor/Models.wl"
<< "Compressor/Arithcoder.wl"
<< "Compressor/Interpreter.wl"
