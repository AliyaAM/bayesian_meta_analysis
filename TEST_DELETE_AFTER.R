#TESTING NEW BAYEs update fUNCTION 

source(paste(SOURCE_ROOT, "BayesUpdateStepByStep.R", sep=""), local = TRUE) # this function (BayesUpdateStepByStep) runs the Bayesian meta-analysis that combines qualitative and quantitative evidence 

seed = as.integer(Sys.time())

Results_Age = BayesUpdateStepByStep(x =x, Construct = "Age", uncertainty = 10, seed = seed)
Results_BayesianMeta_Analysis = rbind(Results_Age)