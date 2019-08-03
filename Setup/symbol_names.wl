(* ::Package:: *)

Get["Sledgehammer`", Path -> FileNameJoin[{NotebookDirectory[], "..", ".."}]];

BeginPackage["Sledgehammer`"];
(* Exported symbols added here with SymbolName::usage *)

Begin["`Private`"];

$names = Block[{$ContextPath = {"Sledgehammer`", "System`"}}, DeleteDuplicates@Join[Names["Sledgehammer`*"], Names["System`*"], Names[]]];
Put[$names, FileNameJoin[{NotebookDirectory[], "symbol_names.mx"}]]


End[]; (* `Private` *)

EndPackage[]
