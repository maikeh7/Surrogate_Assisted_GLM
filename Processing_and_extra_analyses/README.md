## Extra analyses related to Vecchia approximation and stochastic kriging
This folder contains code for carrying out the exercise comparing our Vecchia approximation and SK at the end of Section 3.3 in the manuscript. The files necessary for 
the exercise in section 3.3 are `VecchiaforGLM_MCMC.R` (results for Vecchia approximation) and `SKMCMC.R` (results for SK) and `fit_sk.R`. The plot in Section 3.3 showing
the comparison of CIs/PIs for both methods can be done with `Plot_SK_Vecchia_results.R`. 

The file `construct_GLM_data.R` may be used to construct the GLM dataset for the GLM comparator in Section 5.2.
