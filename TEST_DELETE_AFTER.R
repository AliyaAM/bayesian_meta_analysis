#TESTING NEW BAYEs update fUNCTION 



## Set the root directory to look for source code.
SOURCE_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"

source(paste(SOURCE_ROOT, "BayesUpdateStepByStep.R", sep=""), local = TRUE) # this function (BayesUpdateStepByStep) runs the Bayesian meta-analysis that combines qualitative and quantitative evidence 

seed = as.integer(Sys.time())

Results_Age = BayesUpdateStepByStep(x =x, Construct = "Age", uncertainty = 10, seed = seed)
Results_BayesianMeta_Analysis = rbind(Results_Age)