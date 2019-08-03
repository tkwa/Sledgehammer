(* ::Package:: *)

(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA, see http://wlplugin.halirutan.de/ *)

(* :Title: Sledgehammer *)
(* :Context: Sledgehammer` *)
(* :Author: Thomas *)
(* :Date: 2019-08-01 *)

(* :Package Version: 0.6-dev *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2019 Thomas *)
(* :Keywords: *)
(* :Discussion: *)

$RecursionLimit = 4096;


(* Load all package fragments following https://mathematica.stackexchange.com/a/176489/61597 *)
Sledgehammer`Private`$PackageDirectory = DirectoryName[$InputFileName];
PrependTo[$Path, Sledgehammer`Private`$PackageDirectory];

Sledgehammer`Private`$spf = Get[FileNameJoin@{Sledgehammer`Private`$PackageDirectory, "Setup", "spf.mx"}];
Sledgehammer`Private`$names = Get[FileNameJoin@{Sledgehammer`Private`$PackageDirectory, "Setup", "symbol_names.mx"}];

If[! ValueQ@Sledgehammer`Private`$PackageNames,
  Sledgehammer`Private`$PackageNames = {
    "System`", "Internal`", "GeneralUtilities`",
    "Combinatorica`", "Quaternions`", "FiniteFields`", "Experimental`",
    "Sledgehammer`"}
];

Scan[Needs, {"Combinatorica`", "Quaternions`", "FiniteFields`", "Experimental`"}];
$ContextPath = DeleteCases[$ContextPath, "Combinatorica`"];

<< "Declarations.wl";

<< "Functions/Arrays.wl";
<< "Functions/Automata.wl";
<< "Functions/Constants.wl";
<< "Functions/Entities.wl";
<< "Functions/ExternalLangs.wl";
<< "Functions/Functional.wl";
<< "Functions/General.wl";
<< "Functions/Lists.wl";
<< "Functions/Math.wl";
<< "Functions/Sets.wl";

<< "SystemOverloads/Unprotects.wl";
(*
<< "SystemOverloads/Currying.wl"
<< "SystemOverloads/Operators.wl"
<< "SystemOverloads/String.wl"
*)

<< "Compressor/Models.wl";
<< "Compressor/Arithcoder.wl";
<< "Compressor/Interpreter.wl";