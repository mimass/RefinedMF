module PopulationAnalysis.MeanField
open PopulationAnalysis

open MathNet.Numerics.Distributions
open FSharp.Collections
open FSharp.Control
open DiffSharp.Symbolic.Float64
open DiffSharp.AD.Float64
//open Deedle
open PopulationAnalysis.GossipModel

(* This is the gossip RMF Fsharp version of 28/02/2020 *)


// Functions for Refined Mean Field
// jacobian of phi (model)
let jac = jacobian model_m

// jacobian of h-function
let jac_h = jacobian h

// hessian of h-function
let hes_h = hessian (fun (m:DV) -> (h m).[0])

// subfunctions of Gamma
let G1 (om:DV) (K:DM) (i:int) (j:int) = List.sum [for p in 0..(om.Length-1) -> om.[p]*K.[p,j] * (1-K.[p,j])]
let G2 (om:DV) (K:DM) (i:int) (j:int) = List.sum [for p in 0..(om.Length-1) -> -om.[p]*K.[p,i] * K.[p,j]]

let Gamma (om:DV) (K)=
    toDM [for i in 0..(om.Length-1) ->
              seq[for j in 0..(om.Length-1) ->
                    if (i=j) then G1 om (K om) i j
                             else G2 om (K om) i j ]]

// Initialisation Refined Mean Field
let initD = DM.zeroCreate (im.Length) (im.Length)
let initC = DV.zeroCreate (im.Length)

// Mean field
let rec mfres (h:DV->DV) (om:DV) (K:DV->DM) (n:int) (k:int) (wr:System.IO.StreamWriter) =
    //printfn "occ meas vec om step n %d %f %A %f" (n) (convert (DV.sum om)) (convert om) (convert (K om).[0,0])
    if (n=k) then printfn "Classic MF done"             
                  [] 
                  else sprintf "%s %s \n" (n.ToString()) (((convert ((h om).[0]))*(float N)).ToString()) |> wr.Write
                       (h om) :: (mfres h (om*(K om)) K (n+1) k wr)

// Refined Mean Field. Function call: rmfres h im im K initC initD 0 k

// Preliminary definitions with options for printing intermediate results (commented)
let hestensor = printfn "hestensor"
                List.map (fun (f:DV -> D) -> hessian f) model

let hestenm (m:DV) = //printfn "hestenm"
                     List.map (fun (f:DV -> DM) -> f m) hestensor
let hestensor2 (m:DV) (dmat:DM) = //printfn "hestensor2"
                                  List.map ((.*) (0.5*dmat))  (hestenm m)
let hestensor3 (m:DV) (dmat:DM) = //printfn "hestensor3"
                                  List.map (DM.sum) (hestensor2 m dmat)
let sumhesvec (m:DV) (dmat:DM) = //printfn "sumhesvec"
                                 toDV (hestensor3 m dmat)

let rec rmfres (h:DV->DV) (om:DV) (K:DV->DM) (prevC:DV) (prevD:DM) (n:int) (k:int) (wr:System.IO.StreamWriter) = 
               //printfn "occ meas vec om step n %d %f %A" (n) (convert (DV.sum om)) (convert om)
        if (n=k) then printfn "Refined MF done"
                      []
               else
               let C = (jac om)*prevC+(sumhesvec om prevD)
               let D = (Gamma om K) + (jac om)*prevD*(DM.transpose (jac om))

               let newrmf = (h om) + ((jac_h om)*prevC + (DM.sum ((0.5*prevD) .* (hes_h om))))/ (float) N

               sprintf "%s %s %s \n" (n.ToString()) (((convert (newrmf.[0]))*(float N)).ToString()) ((convert (sqrt(prevD.[1,1])*sqrt((float) N))).ToString()) |> wr.Write
    
               if (n % 100 = 0) then printfn "RMF round %d" n // %A" n (convert om)
                                    
               (newrmf) :: (rmfres h (om*(K om)) K C D (n+1) k wr)
               
// Functions for individual simulation: occupancy measure M and K matrix
let nextPop (M:int list) (K:DV->DM) =
    let mutable newPop = [for i in 0..(List.length M-1) -> (int) 0]  in  // create a list of zeros
    let floatPopVec = List.map (float)  M in                       // turn population vector in floats vector
    let normFloatPopVec = List.map (fun x -> x / ((float) (List.sum M))) floatPopVec in           // normalise dividing by N
            for i in 0..((List.length M)-1) do
            let ratios = convert (K(toDV normFloatPopVec).[i,*])
            // prepare ratios for multinomial
            let mnDist = Multinomial(ratios, M.Item i)
            // instantiate multinomial distribution
            newPop <- List.map2 (+) newPop (Array.toList (mnDist.Sample()))
    
    newPop

// Individual simulation: initial occupance measure M, K matrix, counter n, end time k steps
let rec simulate (h:DV->DV) (M:int list) (K:DV->DM) (n:int) (k:int)  =
      if (n=k) then [] else (convert (h (toDV M))) :: (simulate h (nextPop M K) K (n+1) k)

// Initial mean has all zeros
let initAccMean (M:int list) (k:int) = [for i in 0..(k-1) -> [|for j in 0..((List.length M)-1) -> 0.0|]]

// incremental mean of simulations: M initPop; K matrix; n local counter of samples; k end time; nr_samples ; accMean is mean so far
let rec incMeanSim (h:DV->DV) (M:int list) (K:DV->DM) (n:int) (k:int) (nr_samples:int) (accMean:float [] list) =
    if (n % 100 = 0) then printfn "Simulation round %d" n
    let incMean (a:float []) (b:float []) =
              let mutable c = [|for i in 0..(a.Length-1) -> 0.0|] in
              for i in 0..(a.Length-1) do
                  c.[i] <- (float) (a.[i] + ((float)b.[i]-a.[i])/(float) (n+1)) // n+1 because initAccMean has all zeros and should not count
   //               printfn "c.[i] %f" (c.[i])
              (c)
    let newMean = List.map2 (fun x y -> incMean x y) (accMean) (simulate h M K 0 k) in
    if (n=nr_samples) then accMean
    else (incMeanSim h M K (n+1) k (nr_samples) newMean)
