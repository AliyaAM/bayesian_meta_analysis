
library(filenamer)

#paste(SOURCE_ROOT, "HyperPriorData.csv", sep="")
#paste(OUTPUT_ROOT, "seeds_MAPQualQuant.csv", sep="")

## Set the root directory to look for source code.
SOURCE_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"

#run Bayesian meta-analysis sourcing from 10 different seeds separately for two procedures: 

#1) combining qualitative and quantitative evidence: 
source(paste(SOURCE_ROOT, "QualANDQuant_sourcing_AllSeeds.R", sep=""))

#2) updating hyperprior with quantitative evidence only 
source(paste(SOURCE_ROOT, "QuantOnly_Sourcing_AllSeeds.R", sep=""))

#once the above was done, the results for each seed should be saved in files within the sourced scripts above

#reading from this files average over the results from different seeds below: 
source(paste(SOURCE_ROOT, "Mean_overSeeds_QUANT_QUAL.R", sep=""))
source(paste(SOURCE_ROOT, "Mean_overSeeds_QUANT.R", sep=""))

