# The Accuracy of phenology estimators for use with sparsely sampled presence-only observations

Authors: Michael W. Belitz, Elise A. Larsen, Leslie Ries, & Robert P. Guralnick. 

Code written by: Michael W. Belitz

Code to fully reproduce the simulation set-up, analyses, results, and figures presented in this paper. The code used to set-up the simulation can be found in the simulation_set up sub-directory. The scripts used to estimate pheno-metrics using the four estimators can be found in the analyses_scripts sub-directory. Results of these analyses are found in the results sub-directory. The scripts used to make all of the figures can be found in the figures_scripts sub-directory. RMSE and Bias are calculated in the Figures_3&4, Figures_5&6, Figures_S1&2, and Figures_S3&4 scripts. The saved figures are in the figures_outputs sub-directory.

Note parallelization was used in our code. Please adjust accordingly if running on a local machine with fewer available cores. The highest number of cores used in any script is 30. If running on a cluster with >30 cores, all scripts should be able to run from source without needing to change anything.
