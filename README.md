# RefinedMF

**Author**: Mieke Massink

**Date**: April 9, 2020

**Summary**:
This repository provides the Fsharp source code files for the Refined Mean Field analysis of a model of the push-pull gossip protocol.
The related publication is currently under review and will be added in the future.

The theoretical foundation on which this work has been based can be found in the following publication:

* Nicolas Gast, Diego Latella, Mieke Massink. A refined mean field approximation of synchronous discrete-time population models. Perform. Evaluation 126: 1-21 (2018). https://doi.org/10.1016/j.peva.2018.05.002

**Disclaimer**:
This software is an experimental prototype developed as a first proof-of-concept and used by the authors for producing the analysis results for the related publication; consequently, it has not been developed according to general usability criteria. It is available for the only purpose of reproducibility of the main research results. It is not intended to provide a tool or a library for more general use. To that end, the tool should be thoroughly re-engineered and we hope to work on that in the future.

# Brief installation instructions

The software has been developed on MAC OS (Mojave) using Visual Studio for MAC community.

This requires first the installation of:

   * mono-mdk (mono development kit) that can be obtained via brew

After that install:

   * Visual Studio for Mac community version 7.7.3 (build 43) on a suitable Mac
   with Fsharp language binding and .NET Core support and Mac development

Then start Visual Studio and create a new project with name:

   * PopulationAnalysis

Create also a Solution with the same name PopulationAnalysis

Install the following packages via NuGet:

   * Deedle version 1.2.5 target framework net471
   * DiffSharp version 8.7.7 target framework net471
   * FSharp.Core version 4.3.3 target framework net451
   * FSharp.Quotations.Evaluator version 1.0.6 target framework net471
   * MathNet.Numerics version 4.7.0 target framework net471
   * MathNet.Numerics.FSharp version 4.7.0 target framework net471
   * PLplot version 5.13.7 target framework net471

Some packages require additional software to be installed such as OpenBlas. 
These are indicated in the information provided by NuGet.

After this insert the provided FSharp source code files as provided in this repository.

   * The main source file is: Program.fs

   * The gossip model can be found in the file: GossipModel.fs

In GossipModel.fs also the kind of analysis, the parameter values and the paths to where to save the results have to be given.

The set-up also requires Xcode to be installed and an open Terminal window if run from
Visual Studio. If run from command-line one needs to make sure to have libopenblas.dll and libopenblas.dylib in the directory with the source code.

   * Package configuration can be found in: packages.config

**Note**: The above provides the software for the Refined Mean Field analysis of
the gossip model. 
The software used for the orginal Java-based simulation of the  gossip protocol was 
developed and is owned by Rena Bahkshi. We used this software to compare our
results with the original published results in the literature. 
Since this software is not ours we do not include it
here. For the purpose of reproducibility of the analysis we include the directory RESULTS, 
where simulation traces of the refined mean field and Java simulator can be found for
gossip models with 100 and 2500 nodes and with gmax equal to 3 and 9 respectively. This
directory also includes gnuplot scripts to visualise the results.
