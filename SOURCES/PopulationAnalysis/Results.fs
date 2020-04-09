module PopulationAnalysis.Results
open PopulationAnalysis

open DiffSharp.Symbolic.Float64
open DiffSharp.AD.Float64
open FSharp.Control
open PLplot
open PopulationAnalysis.GossipModel
open PopulationAnalysis.MeanField

(* This is the gossip RMF Fsharp version of 28/02/2020 *)


// Preparation for plotting the results with the PLPlot package


// The following shows only the mean field results of state la
let t, omla =
    let wr1 = new System.IO.StreamWriter(cmf_csv_file)
    let mfla = printfn "Start Classic MF"
               (mfres h im K 0 k wr1) in
    wr1.Close()
    Array.init k (fun i ->
       let t = float i
//       let omla = (convert ((mfres im K 0 k).[i]).[0])
       let omla = (convert (mfla.[i]).[show_local_state])
       //printfn "%i %f" (i) (omla*(float N))
       (t,omla))
      |> Array.unzip


let t2, rla =
    //let wr = new System.IO.StreamWriter(rmf_csv_file)
    let wr2 = new System.IO.StreamWriter(rmf_csv_file)
    let rmfla = printfn "Start Refined MF"
                (rmfres h im K initC initD 0 k wr2) in
    wr2.Close()
    Array.init k (fun i ->
       let t2 = float i
//       let rla = (convert ((rmfres im im K initC initD 0 k).[i]).[0])
//       let rna = (convert ((rmfres im im K initC initD 0 k).[i]).[1])
       let rla = (convert (rmfla.[i]).[show_local_state])
       //printfn "%i %f" (i) (rla*(float N))
       //sprintf "%s %s \n" (t2.ToString()) (rla.ToString()) |> wr.Write
       //wr.Flush()
       (t2, rla))
      |> Array.unzip
 


// Show mean of many simulation traces
// TBD: Test also std deviation of simulation
let t3, simla =
//    let mtrace = (incMeanSim h initPop K 0 k nr_sims ((initAccMean initPop k)))
    let wr3 = new System.IO.StreamWriter(smf_csv_file)
    let mtrace = (incMeanSim h_sim initPop K 0 k nr_sims ((initAccMean [0] k)))
    Array.init k (fun i ->
        let t3 = float i
//        let simla = (float) ((incMeanSim initPop K 0 k nr_sims ((initAccMean initPop k))).Item i).[0]/ (float) N
        let simla = (mtrace.Item i).[show_local_state] / (float) N
        sprintf "%f %f \n" t3 (mtrace.Item i).[show_local_state] |> wr3.Write
        wr3.Flush()
        (t3,simla))
       |> Array.unzip
    



let generateGraph argv =
// Prepare for pdf generation
    let mf = new PLStream()
    printfn "Start set background \n"
    mf.scolbg(255,255,255)
    printfn "Start color map def \n"
    mf.scmap0 ([|255;0;255;0|], [|255;0;0;0|] ,[|255;0;0;255|])
    mf.sdev("pdfcairo")
    mf.setopt("-o",res_file_path) |> ignore
    mf.init()
    mf.env(xminmf, xmaxmf, yminmf, ymaxmf, AxesScale.Independent, AxisBox.BoxTicksLabelsAxes )
    let PL_LEGEND_LINE = LegendEntry.Line
    let PL_LEGEND_SYMBOL = LegendEntry()
    let PL_LEGEND_WIDTH = LegendEntry()
    let p_legend_width = ref 0.2
    let p_legend_height = ref 0.3
    let p_legend_opt = Legend()
    let bxline = LineStyle.Continuous
    let legend_opt = Legend()
    let p_legend_pos = Position()  // Position of the legend. () is default up-right. .Bottom is in the bottom
//    let opt_array = [| PL_LEGEND_WIDTH; PL_LEGEND_LINE; PL_LEGEND_SYMBOL |] in
    let opt_array = [|PL_LEGEND_LINE; PL_LEGEND_LINE; PL_LEGEND_LINE|]
    let text_colors = [| 1; 2; 3 |] in
    let text = [| legend_text_1; legend_text_2; legend_text_3 |] in
    let line_colors = [| 1; 2; 3 |] in
    let line_styles = [| LineStyle.Continuous; LineStyle.ShortDashesShortGaps; LineStyle.ShortDashesLongGaps|] in
    let line_widths = [| line_width; line_width; line_width |] in    //width of lines in legend
    let symbol_colors = [|1;2;3|] in
    let symbol_scales = [|3.0; 3.0; 3.0|] in
    let symbol_numbers = [|1;2;3|] in
    let symbols = [|"mf";"rmf";"sim"|]
    (* note from the above opt_array the first symbol (and box) indices do not matter *)
    mf.legend (
            p_legend_width, p_legend_height, p_legend_opt, p_legend_pos,
            0.0, 0.0, 0.1, 15,
            1, bxline, 0, 0,
            opt_array,
            1.0, 1.0, 2.0,
            1.0, text_colors, text,
            [|1;2;3|], [|Pattern();Pattern();Pattern()|], [|1.0;1.0;1.0|], [|1.0;1.0;1.0|],
            line_colors, line_styles, line_widths,
            symbol_colors, symbol_scales, symbol_numbers, symbols
        );
    mf.lab(x_axes_text,y_axes_text,graph_title)
    mf.col0 1
    mf.lsty LineStyle.Continuous
    mf.width line_width   // This makes all lines in the graph of width 2.0
    mf.line(t, omla)  // mf.line(t,omla) mean field of la
    //printfn "%A" (t,omla)
    mf.lsty LineStyle.ShortDashesShortGaps
    mf.col0 2
    mf.line(t2,rla) // mf.line(t2, rla) refined mean field of
    //printfn "%A" (t2,rla)
    mf.col0 3
    mf.lsty LineStyle.ShortDashesLongGaps
    mf.line(t3,simla)  // mf.line(t3,simla) simulation
    mf.font FontFlag.Sans
    mf.fontld(3)
// end PLPlot package code
    0 // Return an integer exit code

