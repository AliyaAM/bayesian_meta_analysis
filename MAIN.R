
library(tidyverse)
library(dplyr)
library(assertthat)
library(ggplot2)
library(filenamer)
library(reshape2)  
library(tibble)
library(compute.es)
library(metafor)
library(bayesplot)
library(ggplot2)
library(rstan) 
library(coda)
library(bayestestR)
library(HDInterval)
library(assertthat)

print("")
#paste(SOURCE_ROOT, "HyperPriorData.csv", sep="")
#paste(OUTPUT_ROOT, "seeds_MAPQualQuant.csv", sep="")
#paste(OUTPUT_ROOT, logName, "/Results_ResultsBayesianUpdateQuant.csv", sep="")
x = read.csv(paste(SOURCE_ROOT, "input.csv", sep="")) #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 
data = read.csv(paste(SOURCE_ROOT, "QuantData_CheckedForAccuracy_20March2020.csv", sep=""))  #data extracted from from the quantitative studies 
JaarsmaInternationalStudy = read.csv(paste(SOURCE_ROOT, "HyperPriorData.csv", sep="")) #data used for eliciting the hyperprior (general physical activity levels in HF estimated from a large internaitonal study (Jaarsma et al., 2013)


## Set the root directory to look for source code.
SOURCE_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"

#data 
x = read.csv(paste(SOURCE_ROOT, "input.csv", sep=""))  #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 
data = read.csv(paste(SOURCE_ROOT, "QuantData_CheckedForAccuracy_20March2020.csv", sep=""))  #data extracted from from the quantitative studies 
JaarsmaInternationalStudy = read.csv(paste(SOURCE_ROOT, "HyperPriorData.csv", sep="")) #empirical hyperprior 

#run Bayesian meta-analysis sourcing from 10 different seeds separately for two procedures: 

#1) combining qualitative and quantitative evidence: 
Bayes_update_Qual_quant_noHyperprior.R

source(paste(SOURCE_ROOT, "QualANDQuant_sourcing_AllSeeds.R", sep=""))

#2) updating hyperprior with quantitative evidence only 
source(paste(SOURCE_ROOT, "QuantOnly_Sourcing_AllSeeds.R", sep=""))

#once the above was done, the results for each seed should be saved in files within the sourced scripts above

#reading from this files average over the results from different seeds below: 
source(paste(SOURCE_ROOT, "Mean_overSeeds_QUANT_QUAL.R", sep=""))
source(paste(SOURCE_ROOT, "Mean_overSeeds_QUANT.R", sep=""))

#plot the findings of the anlaysis of quantitative evidence (i.e., hyperprior was updated with quantitative evidence)
source(paste(SOURCE_ROOT, "Bayesian_MA_plotting_quantitative_findings.R", sep=""))

#plot the findings of the analysis combining qualitative and quantitative evidence (i.e., hyperprior was updated with prior elicited from the experts, then the resultant distribution was updated with the quantitative evidence)
#note: fewer constructs were evaluated in both qualitative and quantitative evidence 
source(paste(SOURCE_ROOT, "Bayesian_MA_plotting_Qual_plus_QUANT_findings.R", sep=""))

#plot the fidings with and without qualitative evidence next to each other for comparison
source(paste(SOURCE_ROOT, "Bayesian_MA_plot_Compare_Quant_Qual", sep=""))



