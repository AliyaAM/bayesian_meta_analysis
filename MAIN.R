#install.packages("RColorBrewer")

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
library(ggridges)
library(rstan) 
library(coda)
library(bayestestR)
library(HDInterval)
library(assertthat)
library(RColorBrewer)

## Set the root directory to look for source code.
SOURCE_ROOT = "//Users/aliyaamirova/proj/bayesian_meta_analysis/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliyaamirova/proj/bayesian_meta_analysis/"


x = read.csv(paste(SOURCE_ROOT, "input.csv", sep="")) #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 
data = read.csv(paste(SOURCE_ROOT, "QuantData_CheckedForAccuracy_20March2020.csv", sep=""))  #data extracted from  the quantitative studies, the file lists all data including the data that was not used for the meta-analysis. the data not included in the meta-anslysis is for the cases when insufficient data was reported in the article for it to be pooled in the meta-analysis (for example mean but no SD or variance etc)
JaarsmaInternationalStudy = read.csv(paste(SOURCE_ROOT, "HyperPriorData.csv", sep="")) #data used for eliciting the hyperprior (general physical activity levels in HF estimated from a large internaitonal study (Jaarsma et al., 2013)



source(paste(SOURCE_ROOT, "ConvertEffectsizes.R", sep="")) #### convert effect sizes from individual studies  (F-value, Binary (Absolute numbers and proportions), r coeffcient and SMD) into log odds ratios. All quantitative results are converted to log OR in order to be comptable with qualitative evidence, we treated all results as binary. 
likelihood_data =  ConvertEffectsizes(data = data)


#run Bayesian meta-analysis for two procedures separately: 

#on the constructs that were present in both qualitative and quantitative studies (the main plot is outputed from the function below): 
source(paste(SOURCE_ROOT, "Bayesian_MA_Quant_and_Qual.R", sep=""))



#constructs that were present in quantitative studies only (the main plot is outputed from the function below): 
source(paste(SOURCE_ROOT, "BayesianMA_Quant_only.R", sep=""))



#plot the fidings with and without qualitative evidence next to each other for comparison

source(paste(SOURCE_ROOT, "Compare_Qual_Quant_posterior.R", sep=""))
