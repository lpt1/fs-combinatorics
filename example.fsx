
#r @"bin/Debug/netstandard1.6/fs-combinatorics.dll"

open FSCombinatorics

let permutations = Cartesian.repeat [0; 1] 4

let compositions = Compositions.intCompositions 4