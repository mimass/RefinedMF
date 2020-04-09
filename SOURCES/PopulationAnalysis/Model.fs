module PoAN.WSNModel
(*

open PoAN

open FSharp.Core
open DiffSharp.Symbolic.Float64
open DiffSharp.AD.Float64
open Deedle

// WSN model

//Parameters WSN model
let Einit = 100
let Ainit = 50
let N = Einit + Ainit

let q = 10.0

let be = 9.0/q
let al = 0.9/q
let la = 0.9/q
let et = 0.1/q
let ga = 0.1/q

let k = 500 // number of time steps

let initPop = [Ainit;0;0;0;Einit]
let im = toDV [(float) Ainit/(float) N;0.0;0.0;0.0;(float) Einit/(float) N]

let nr_sims = 1000

let K (m:DV) = toDM
                [[1.0-be*m.[2];be*m.[2];(DV.create 1 0.0).[0];(DV.create 1 0.0).[0];(DV.create 1 0.0).[0]]
                 [(DV.create 1 al).[0];(DV.create 1 (1.0-al)).[0];(DV.create 1 0.0).[0];(DV.create 1 0.0).[0];(DV.create 1 0.0).[0]]
                 [(DV.create 1 0.0).[0];(DV.create 1 0.0).[0];1.0-ga-be*m.[0];(DV.create 1 ga).[0];be*m.[0]]
                 [(DV.create 1 0.0).[0];(DV.create 1 0.0).[0];(DV.create 1 et).[0];(DV.create 1 (1.0-et)).[0];(DV.create 1 0.0).[0]]
                 [(DV.create 1 0.0).[0];(DV.create 1 0.0).[0];(DV.create 1 la).[0];(DV.create 1 0.0).[0];(DV.create 1 (1.0-la)).[0]]]
let gwa (m:DV) = m.[0] - be*m.[2]*m.[0] + al*m.[1]
let gwb (m:DV) = m.[0]*be*m.[2] + m.[1] - m.[1]*al
let wsnc (m:DV) = m.[2]-ga*m.[2]-be*m.[0]*m.[2] + et*m.[3] + la*m.[4]
let wsnd (m:DV) = ga*m.[2] + m.[3] - et*m.[3]
let wsne (m:DV) = be*m.[0]*m.[2] + m.[4]-la*m.[4]

let model_m (m:DV) = toDV [gwa m;gwb m;wsnc m;wsnd m;wsne m]

let model = [gwa;gwb;wsnc;wsnd;wsne]

let h (m:DV) = toDV [(m.[2]+ m.[3])/(la*(m.[4]+(1.0/(float) N)))]  // normalised for mean field
let h_sim (m:DV) = toDV [(m.[2]+ m.[3])/(la*(m.[4]+1.0))]        // non-normalised for simulation
*)

(* MRDL model
// Parameters MRDL model
let N = 320.0    // population size
let p = 0.6      // initial fraction with opinion A
let nA = p*N     // initial number with opinion A
let nB = (N-nA)  // initial fraction with opinion B

let lambda = 1.0
let q = 10.0
let k = 200      // time horizon

let nr_sims = 1000  // number of simulation traces

let initPop = [(int) nA;0;(int) nB;0]           // initial population vector
let im = toDV [nA/N;0.0;nB/N;0.0]   // initial occupancy measure vector

// The following defines the K matrix that depends on the occupancy measure vector m:
let K (m:DV) = toDM
                [[(DV.create 1 ( 1.0) - ( 1.0/ q)).[0];(DV.create 1 (1.0/q)).[0];(DV.create 1  0.0).[0];(DV.create 1  0.0).[0]];
                [(3.0/q)*(m.[1]*m.[1]+m.[1]*m.[3]);(1-(3.0/q)*(m.[1]*m.[1]+m.[1]*m.[3])-(3.0/q)*m.[3]*m.[3]);(3.0/q)*m.[3]* m.[3];(DV.create 1 0.0).[0]];
                [(DV.create 1 0.0).[0];(DV.create 1 0.0).[0];(DV.create 1 (1.0-(lambda/q))).[0];(DV.create 1 (lambda/q)).[0]];
                [(3.0/q)*(m.[1]* m.[1]);(DV.create 1 0.0).[0];(3.0/q)*(m.[3]*m.[3]+m.[1]*m.[3]);(1-(3.0/q)*m.[1]*m.[1]-(3.0/q)*(m.[3]*m.[3]+m.[1]*m.[3]))]]

let la (m:DV) = m.[0] - ((1.0/q) * m.[0]) + (3.0/q)*(m.[1]*m.[1]*m.[1] + m.[1]*m.[1]*m.[3]) + (3.0/q)*m.[1]*m.[1]*m.[3]
let na (m:DV) = m.[1] - (3.0/q)*(m.[1]*m.[1]*m.[1] + m.[1]*m.[1]*m.[3]) - (3.0/q)*m.[3]*m.[3]*m.[1] + (1.0/q)*m.[0]
let lb (m:DV) = m.[2] - (lambda/q)*m.[2] + (3.0/q)*m.[3]*m.[3]*m.[1] + (3.0/q)*(m.[3]*m.[3]*m.[3] + m.[1]*m.[3]*m.[3])
let nb (m:DV) = m.[3] - (3.0/q)*m.[1]*m.[1]*m.[3] - (3.0/q)*(m.[3]*m.[3]*m.[3] + m.[1]*m.[3]*m.[3]) + (lambda/q)*m.[2]

//let mrdl (m:DV) = DV.create 4 [la m, na m, lb m, nb m]
let model_m (m:DV) = toDV [la m; na m; lb m; nb m]

let model = [la;na;lb;nb]

let h (m:DV) = toDV [m.[0]+ m.[1]]  // fraction of population with opinion A

*)

(*
// (some) Parameters of the pdf graph to show the result with "generateGraph"
let show_local_state = 0  // When using h-function the only item to show is 0
let legend_text_1 = "Mean Field Response time"
let legend_text_2 = "Refined Mean Field Response time"
let legend_text_3 = "Simulation Response time"
let graph_title = ""
let x_axes_text = "time"
let y_axes_text = "delay"
let line_width = 1.0
let res_file_path = "/Users/common/Desktop/PoAN_results/PoANrefmeanfieldtest.pdf"

let xminmf, xmaxmf, yminmf, ymaxmf = 0.0, (float) k, 0.0, 35.0

// Note 1: in principle model = m * (K m) but unfortunately that gives slightly different numeric results. Not clear why.
//let model = [(fun (m:DV) -> (m * (K m)).[0]);(fun (m:DV) -> (m * (K m)).[1]);(fun (m:DV) -> (m * (K m)).[2]);(fun (m:DV) -> (m * (K m)).[3])]
*)