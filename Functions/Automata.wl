(* ::Package:: *)

BeginPackage["Sledgehammer`"]
Begin["`Private`"]



(*
Examples: Hungry Mouse, Langton's Ant
g inputs: state \[Rule] cell \[Rule] pos \[Rule] surrounding 3x3 box \[Rule] whole grid \[Rule] dir \[Rule]
g outputs: {newState, newCell, turn}
newState: change state
newCell: change cell
turn: turn as a multiple of 90 degrees CCW
*)

Clear[Turmite2DBounded]

Turmite2DBounded[g_Function, stateSpec_, gridSpec_: {}, n_, OptionsPattern[]] := Module[
{f = Function[{grid, state, dir, pos},
	Module[{newState, newCell, turn, newDir, neighbors, cell}, 
	cell = grid[[Sequence@@Mod[pos,Dimensions@grid,1] ]];
	neighbors = ArrayPad[grid, 1, "Periodic"][[pos[[1]];; pos[[1]] + 2, pos[[2]];; pos[[2]] + 2]];
	{newState, newCell, turn} = g[state, cell, pos, neighbors, grid, dir];
	newDir = ReIm[Complex@@dir * Round[I^turn]] ;
	{ReplacePart[grid, pos -> newCell], newState, newDir, Mod[pos + newDir, Dimensions@grid, 1]}
]], startState, startPos, startDir, startGrid},

	{startState, startPos, startDir} = Replace[stateSpec, {
		(s_? IntegerQ) | {s_? IntegerQ} :> {s, {1, 1}, {1,0}},
		{s_? IntegerQ, p_} :> {s, p, {1, 0}},
		{s_? IntegerQ, p_, d_} :> {s, p, d},
		_ :> Throw["Invalid stateSpec "@stateSpec]
		}];


	startGrid = Replace[gridSpec, {
		grid_List /; ArrayDepth@grid == 2 :> grid,
		x_? IntegerQ :> ConstantArray[0, {x, x}],
		{x_? IntegerQ} :> ConstantArray[0, {1, x}],
		{x_? IntegerQ, y_? IntegerQ} :> ConstantArray[y, {x, x}],
		{} -> ConstantArray[0, {n,n}]
	}];
	startPos = Replace[startPos, {(p_? IntegerQ /; p >= 1 ):> {p, p} , Center -> Quotient[Dimensions@startGrid + 1, 2]}];
	startDir = Replace[startDir, d_? IntegerQ :> ReIm[I^d]];

	NestList[Apply[f],
		{
			startGrid,
			startState,
			startDir,
			startPos
		},
		n]
];
Turmite2DBoundedResult[args___] := Turmite2DBounded[args][[-1,1]]


(*
Examples: Hungry Mouse, Langton's Ant
g inputs: state \[Rule] cell \[Rule] pos \[Rule] surrounding 3x3 box \[Rule] whole grid \[Rule] dir \[Rule]
g outputs: {newState, newCell, newDir}
newState: change state
newCell: change cell
dir: new direction as ordered pair
*)

Mouse2DBounded[g_Function, stateSpec_, startGrid_, n_, OptionsPattern[]] := Module[
{f = Function[{grid, state, dir, pos},
	Module[{newState, newCell, newDir, neighbors, cell}, 
	cell = grid[[Sequence@@Mod[pos,Dimensions@grid,1] ]];
	neighbors = ArrayPad[grid, 1, "Periodic"][[pos[[1]];; pos[[1]] + 2, pos[[2]];; pos[[2]] + 2]];
	{newState, newCell, newDir} = g[state, cell, pos, neighbors, grid, dir];
	{ReplacePart[grid, pos -> newCell], newState, newDir, Mod[pos + newDir, Dimensions@grid, 1]}
]], startState, startPos, startDir},

	{startState, startPos, startDir} = Replace[stateSpec, {
		(s_? IntegerQ) | {s_? IntegerQ} :> {s, {1, 1}, {1,0}},
		{s_? IntegerQ, p_} :> {s, p, {1, 0}},
		{s_? IntegerQ, p_, d_} :> {s, p, d},
		_ :> Throw["Invalid stateSpec "@stateSpec]
		}];

	startPos = Replace[startPos, {(p_? IntegerQ /; p >= 1 ):> {p, p} , Center -> Quotient[Dimensions@startGrid + 1, 2]}];
	startDir = Replace[startDir, d_? IntegerQ :> ReIm[I^d]];

	NestList[Apply[f],
		{
			startGrid,
			startState,
			startDir,
			startPos
		},
		n]
];
Mouse2DBoundedResult[args___] := Mouse2DBounded[args][[-1,1]]



End[];
EndPackage[];
