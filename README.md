# ndstools: Routines for Nonlinear Dynamical Systems Data Analyses

(For now, this README is being used as a file to help me keep track of things. I'll clean it up at the end.)

Nonlinear dynamical systems (NDS) is an approach to understanding systems that focuses on how the *state* of the system at one time is influenced by the state of the system at previous times. NDS data analysis can be considered an extension of ideas such as autoregression and cross-lagged panel models, wherein time is put in the background and the notion of iid data is not appropriate. There are many different approaches to NDS data anlaysis; the NDSToolkit is an R package containing a number of functions for nonlinear dynamical systems analyses. Specifically, this package contains or will contain the following functions:

- Burstiness estimations
- orbital decomposition

## Functions in Progress

- CRQA with automatic time series embedding (C++)
- RQA with automatic time series embedding (using Pecora; in C++)
- Markov matrix estimation from time series data (with bootstrapped ci)
- Orbital decomposition
- State Space Grid graphics for sequences of categorical or logical variables (See https://lhama.osu.edu/methods-tutorials/state-space-grids/ for some ggplot plotting code.)
- Text preparation for use in RQA routines
- Time series embedding (C++) - do an auto-encoding based on Pecora for unidimensional time series and xxxx for multi-dimensional time series.
- Bootstrapped ci for power law estimation
- Butterfly catastrophe estimation
- Cusp catastrophe model estimation (and trajectory plots/analysis); look at Butner et al.
- Early warning signal methods for EMA data
- Entropy calculations (and EWS/PTDA implementation)
- Nonlinear estimation for network psychometrics (in C++)
- pseudo-Attractor and trajectory creation
- Probability surfaces for formal models
- State space metrics (following Hollenstein)
- Swallowtail catastrophe model estimation
- Sync (port SyncCalc, MEA?)
- SINDy with latent classes (C++) - put it here, adapted for work with noisy datasets (although see also *package::sindyr*)
- Koompan mode decomposition (C++)
- Best derivatives estimation
- PTDA
- Need vignettes

## Datasets

This package also contains a number of datasets

- Sample categorical sequence from Guastello (2011)
- A sample of categorical time series data from Ricca & Jordan

## Functions found elsewhere

This package fills in gaps in the existing *R* packages that perform NDS analyses. Although some of the existing approaches may deserve a wrapper or a C++ update (for speed), because these approaches already exist in other packages, they are not included in *ndstools*. Here, however, is a (incomplete, I'm sure) listing of other tools that can be useful in NDS.

- bootstrapped *burstiness confidence intervals* may be obtained via boot::tsboot(ts, statistic = event_burstiness, R = 100). In fact, boot::tsboot() will provide bootstrapped c.i. for many statistics.
- package::markovchain creates *transition (a.k.a. "Markov") matrices* with bootstrapping s.e. and includes many functions that rely on these transition matrices
- *dynamic time warping*
- *topological data analysis* tools can be found in several packages.
- *kalman filters* are implemented using...
- parameters of *power law distributions* may be estimated using *igraph::fit_power_law*
- *Convergent cross mapping* can be found in *package::rEDM*
- Approaches to *detrended fluctuation analysis* (DFA) and *multifractal detrended fluctuation analysis* (MFDFA) can be found in *package::DFA* and *package:MFDFA*, respectively
- Although not usually implemented with nonlinear basis vectors, growth mixture modeling (GMM), parallel process growth mixture modeling (ppGMM), and latent growth curve modeling (LGCM) can be nonlinear analyses. And although GMM, etc., typically foreground time and so are not strictly dynamic approaches, *dynamical systems analysis* (DSA) relies on these techniques. These are implemented using *package:lcmm* and its functions lcmm and multlcmm; hlme can be used to assist with convergence problems in lcmm. (These approaches all suffer from convergence issues and often need extremely long computational times. Better starters and convergence criteria should be implemented with these, and a port to OpenCL to take advantage of GPU would be really helpful. Those are both on my to-do list.)
- TISEAN (https://www.mpipks-dresden.mpg.de/tisean/Tisean_3.0.1/index.html) is a suite of tools for nonlinear TIme SEries ANalysis; its corresponding R package is no longer maintained and has been removed from CRAN.
- Although not strictly nonlinear, network psychometrics has some useful tools and it just begging for the inclusion of nonlinear estimation approaches. A brief overview of network psychometrics is available at https://rpubs.com/SamLar97/network-psychometrics-intro; that site cites an excellent (and exhaustive) book written by those involved in the *R* package
- GIMME needs more exploration by me, but I don't want to forget about it.

