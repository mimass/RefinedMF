module PopulationAnalysis.GossipModel
open PopulationAnalysis

open FSharp.Core
open DiffSharp.Symbolic.Float64
open DiffSharp.AD.Float64

(* This is the gossip RMF Fsharp version of 28/02/2020 of
the 6-state model of the COORDINATION 2020 submission in Fig. 7.
Note 1: Parameter values and kind of analysis (replication or coverage) must be set below.
Note 2: Proper directories for simulation and MF results must be set below. 
*)


//Parameters Gossip model
let N = 2500 // number of objects
let n = 500.0  // number of different data items 500, 1000 or 2000
let c = 100.0  // size of cache
let s = 50.0   // number of items exchanged

// kind of analysis
let analysis_kind = "replication" // "coverage" or "replication"

let gmax=9.0

let initd = 1 // having data element initially

// constant probabilities data item exchange: in Coordination 2020 paper D=1 and O=0 in defs below:
let P_11_01 = (D (s/c)*((c-s)/(n-s))) // P_a2b2_a1b1 A gets d from B and B keeps a copy
let P_10_01 = (D (s/c)*((n-c)/(n-s)))
let P_01_10 = P_10_01
let P_01_11 = (D (s/c)*((c-s)/c)*((n-c)/(n-s)))
let P_11_11 = (D 1.0)-(D 2.0)*P_01_11
let P_10_11 = P_01_11
let P_10_10 = (D ((c-s)/c))
let P_11_10 = P_11_01
let P_00_00 = (D 1.0)
let P_01_01 = P_10_10

// Number of time steps
let k = 3000

// initial population distribution, non-normalised
let initPop = [0;initd-1;N-initd;0;1;0]

// initial population distribution in fractions of total population
let im = toDV [0.0;(float) (initd-1)/(float) N;(float) (N-initd)/(float) N;0.0;(float) 1/(float) N;0.0]

// number of simulation runs. Should be at least 1000 for good approximation
let nr_sims = 1

// No collision probability asymptotic (N to infinity)
let noc = (exp((D -2.0)/(gmax+1.0)))

// transition probability functions depending on occupancy measure vector m
let get_self_rep (m:DV) = 2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[1]+m.[4])*(P_11_10)*(noc)
let get_self_ex (m:DV) = 2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[1]+m.[4])*(P_01_10)*(noc)

let loose_self_rep (m:DV) = 2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[1]+m.[4])*(P_10_11)*(noc)
let loose_self_ex  (m:DV) = 2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[0]+m.[2]+m.[3]+m.[5])*(P_01_10)*(noc)

// transitions in probability matrix K
let tpf (m:DV) (i:int) (j:int)  =
                                  match (i, j) with
                                  | (0,1) -> (get_self_rep m)
                                  | (1,0) -> (loose_self_rep m)
                                  | (0,0) -> (1.0 - (get_self_rep m) - (get_self_ex m))
                                  | (0,5) -> (get_self_ex m) 
                                  | (1,1) -> (1.0 - (loose_self_rep m))
                                  | (2,1) -> (get_self_rep m)
                                  | (2,2) -> (1.0 - (get_self_rep m) - (get_self_ex m))
                                  | (2,3) -> (get_self_ex m)
                                  | (3,0) -> (loose_self_ex m)
                                  | (3,1) -> (get_self_rep m)
                                  | (3,3) -> (1.0 - (loose_self_ex m)- (get_self_rep m))
                                  | (4,4) -> ((DV.create 1  1.0).[0])
                                  | (5,1) -> (get_self_rep m)
                                  | (5,0) -> (loose_self_ex m)
                                  | (5,5) -> (1.0 - (get_self_rep m) - (loose_self_ex m))
                                  | (_, _) -> ((DV.create 1  0.0).[0])
// probability matrix K
let K (m:DV)= DM.init 6 6 (tpf m)

// difference equations compatible with the matrix K made explicit for use with symbolic computation
// used definitions of probability functions get and loose to make symbolic computation possible
let OO (m:DV) = m.[0] - m.[0]*(2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[1]+m.[4])*(P_11_10)*(noc))
                      - m.[0]*(2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[1]+m.[4])*(P_01_10)*(noc)) 
                      + (m.[1])*(2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[1]+m.[4])*(P_10_11)*(noc))
                      + (m.[3]+m.[5])*(2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[0]+m.[2]+ m.[3]+m.[5])*(P_01_10)*(noc))

let DD (m:DV) =(m.[0]+ m.[2]+ m.[3]+m.[5])* (2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[1]+m.[4])*(P_11_10)*(noc)) 
                     + m.[1] - (m.[1])*(2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[1]+m.[4])*(P_10_11)*(noc))

let IO (m:DV) = m.[2]- m.[2]*(2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[1]+m.[4])*(P_11_10)*(noc) +
                              2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[1]+m.[4])*(P_01_10)*(noc))

let OA (m:DV) = m.[3] - m.[3]*(2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[0]+m.[2]+m.[3]+m.[5])*(P_01_10)*(noc)) 
                      + m.[2]*(2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[1]+m.[4])*(P_01_10)*(noc))
                      - m.[3]*(2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[1]+m.[4])*(P_11_10)*(noc))
                     

let PD (m:DV) = m.[4]

let AO (m:DV) = m.[5] + m.[0]*(2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[1]+m.[4])*(P_01_10)*(noc))
                      - m.[5]*(2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[0]+m.[2]+m.[3]+m.[5])*(P_01_10)*(noc))
                      - m.[5]*(2.0 * (gmax / ((gmax+1.0)*(gmax+1.0)))*(m.[1]+m.[4])*(P_11_10)*(noc))

// used to compute symbolic jacobian that requires input values m
let model_m (m:DV) = toDV [OO m;DD m;IO m;OA m;PD m;AO m]

// used in refined mean field function
let model = [OO;DD;IO;OA;PD;AO] 

// Coverage or Replication
let h (m:DV) = if analysis_kind= "coverage" then toDV [m.[0]+m.[1]+m.[3]+m.[4]+m.[5]] // sum of states LD, FD, O, D, PD
                                            else toDV [m.[1]+m.[4]]                   // sum of states PD and D    
let h_sim (m:DV) = if analysis_kind= "coverage" then toDV [m.[0]+m.[1]+m.[3]+m.[4]+m.[5]]  // sum of states LD, FD, O, D, PD
                                                else toDV [m.[1]+m.[4]]                    // sum of states PD and D
                                                       

// (some) Parameters of the pdf graph to show the result with "generateGraph"
let show_local_state = 0  // When using h-function the only item to show is 0
let legend_text_1 = "Classic Mean Field " + analysis_kind
let legend_text_2 = "Refined Mean Field " + analysis_kind
let legend_text_3 = "Simulation " + analysis_kind
let graph_title = ""
let x_axes_text = "time"
let y_axes_text = analysis_kind + " degree" 
let line_width = 1.0
let res_file_path = "/Users/mieke/Desktop/PoAN_results/PoAN_6state_model_COORDINATION2020_" + analysis_kind + "_N2500_sim1.pdf"
// define file name variables where to put the numeric results of analysis
let mutable rmf_csv_file = ""
let mutable cmf_csv_file = ""
let mutable smf_csv_file = ""
if analysis_kind = "replication"
then // In case of replications:
    rmf_csv_file <- "/Users/mieke/Desktop/PoAN_results/rmfReplicas_c100_s50_n500_fsharp.txt"
    cmf_csv_file <- "/Users/mieke/Desktop/PoAN_results/cmfReplicas_c100_s50_n500_fsharp.txt"
    smf_csv_file <- "/Users/mieke/Desktop/PoAN_results/smfReplicas_c100_s50_n500_fsharp.txt"
else if analysis_kind = "coverage"
     then // in case of coverage:
     rmf_csv_file <- "/Users/mieke/Desktop/PoAN_results/rmfCoverage_c100_s50_n500_fsharp.txt"
     cmf_csv_file <- "/Users/mieke/Desktop/PoAN_results/cmfCoverage_c100_s50_n500_fsharp.txt"
     smf_csv_file <- "/Users/mieke/Desktop/PoAN_results/smfCoverage_c100_s50_n500_fsharp.txt"
     else
     printfn "Specify kind of analysis: replication or coverage"
     rmf_csv_file <- ""
     cmf_csv_file <- ""
     smf_csv_file <- ""

// define minimum and maximum values on x-axes and y-axes in generated pdf-plot
let xminmf, xmaxmf, yminmf, ymaxmf = 0.0, (float) k, 0.0, 1.0

// END VERSION 6-state model of COORDINATION 2020 paper (Fig. 7)

