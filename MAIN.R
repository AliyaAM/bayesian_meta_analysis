
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

## Set the root directory to look for source code.
SOURCE_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"


x = read.csv(paste(SOURCE_ROOT, "input.csv", sep="")) #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 
data = read.csv(paste(SOURCE_ROOT, "QuantData_CheckedForAccuracy_20March2020.csv", sep=""))  #data extracted from from the quantitative studies 
JaarsmaInternationalStudy = read.csv(paste(SOURCE_ROOT, "HyperPriorData.csv", sep="")) #data used for eliciting the hyperprior (general physical activity levels in HF estimated from a large internaitonal study (Jaarsma et al., 2013)

#run Bayesian meta-analysis for two procedures separately: 
#on the constructs that were present in both qualitative and quantitative studies: 
source(paste(SOURCE_ROOT, "Bayesian_MA_Quant_and_Qual.R", sep=""))

#and constructs that were present in quantitative studies only: 
source(paste(SOURCE_ROOT, "BayesianMA_Quant_only.R", sep=""))

#plot the findings of the anlaysis of quantitative evidence (i.e., hyperprior was updated with quantitative evidence)
source(paste(SOURCE_ROOT, "Bayesian_MA_plotting_quantitative_findings.R", sep=""))

#plot the findings of the analysis combining qualitative and quantitative evidence (i.e., hyperprior was updated with prior elicited from the experts, then the resultant distribution was updated with the quantitative evidence)
#note: fewer constructs were evaluated in both qualitative and quantitative evidence 
source(paste(SOURCE_ROOT, "Bayesian_MA_plotting_Qual_plus_QUANT_findings.R", sep=""))

#plot the fidings with and without qualitative evidence next to each other for comparison
source(paste(SOURCE_ROOT, "Bayesian_MA_plot_Compare_Quant_Qual", sep=""))



