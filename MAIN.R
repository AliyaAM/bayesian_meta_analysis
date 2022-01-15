
library(filenamer)


SOURCE_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"


#run Bayesian meta-analysis sourcing from 10 different seeds separately for two procedures: 

#1) combining qualitative and quantitative evidence: 
source(paste(SOURCE_ROOT, "QualANDQuant_sourcing_AllSeeds.R", sep=""))

#2) updating hyperprior with quantitative evidence only 
source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/QuantOnly_Sourcing_AllSeeds.R')

#once the above was done, the results for each seed should be saved in files within the sourced scripts above

#reading from this files average over the results from different seeds below: 
source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/Mean_overSeeds_QUANT_QUAL.R')

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/Mean_overSeeds_QUANT.R')

