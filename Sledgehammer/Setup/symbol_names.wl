(* ::Package:: *)

Get["Sledgehammer`", Path -> FileNameJoin[{NotebookDirectory[], "..", ".."}]];

BeginPackage["Sledgehammer`"];
(* Exported symbols added here with SymbolName::usage *)

Begin["`Private`"];

$names = DeleteDuplicates@Join[Names["Sledgehammer`*"], Names["System`*"], Names[]];
Put[$names, FileNameJoin[{NotebookDirectory[], "symbol_names.mx"}]]


End[]; (* `Private` *)

EndPackage[]
