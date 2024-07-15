# NDSToolkit

The NDSToolkit is an R package containing a number of functions for nonlinear dynamical systems analyses. Specifically, this package contains or will contain the following functions:

## Functions Completed

## Functions in Progress

- Burstiness for event-based sequences (needs data for an example; take from Ricca & Jordan)
- Burstiness for time-based sequences
- CRQA with automatic time series embedding (C++)
- RQA with automatic time series embedding (using Pecora; in C++)
- Markov matrix estimation from time series data (with bootstrapped ci)
- Orbital decomposition
- State Space Grid graphics for sequences of categorical or logical variables
- Text preparation for use in RQA routines
- Time series embedding (C++)
- Bootstrapped ci for burstiness
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
- SINDy with latent classes (C++)
- Koompan mode decomposition (C++)
- Best derivatives estimation
- PTDA


This package also contains a number of datasets

- Sample categorical sequence from Guastello (2011)

Note to self: devtools::document() to create help pages from R file.

Need tutorials or examples

Other things that may deserve a wrapper or a C++ update here:

TDA
DTW
Power law fits
GIMME
Psychonetrics
Kalman filters
TISEAN stuff
rEDM / Converngent Cross Mapping
SINDy
DFA and MFDFA
GMM (do via OpenCL) and ppGMM with GOOD STARTERS and BETTER CONVERGENCE CRITERIA
