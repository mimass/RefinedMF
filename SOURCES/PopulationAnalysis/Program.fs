// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

(* This is the gossip RMF Fsharp version of 28/02/2020 *)

module PopulationAnalysis.Main
open PopulationAnalysis

open PLplot

[<EntryPoint>]
let main argv = 
    let x = PopulationAnalysis.Results.generateGraph argv
    printfn "Results produced in pdf file"
    x

