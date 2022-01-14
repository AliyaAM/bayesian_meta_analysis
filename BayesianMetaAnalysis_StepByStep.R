
library(tidyverse)
library(dplyr)
library(assertthat)
library(ggplot2)
library(reshape2)  
library(filenamer) 

x = read.csv('/Users/aliya/my_docs/proj/bayesian_meta_analysis/input.csv')  # requires this data for all indexed functions that read information for each construct across studies 
print(x)

data = read.csv('/Users/aliya/my_docs/proj/bayesian_meta_analysis/QuantData_CheckedForAccuracy_20March2020.csv') 
print(data) #data from the quant studies 

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/BayesUpdateStepByStep.R', local = TRUE)

source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/VariableType.R')


###################################### THE BAYES UPDATE three steps ###########
########################################################################### Bayes update Jaarsma Hyper prior + quali studies (expert elicitation) + quant studies = posterior 

print("uncertainty")
print(uncertainty)



Results_Age = BayesUpdateStepByStep(x =x, Construct = "Age", uncertainty = uncertainty, seed = seed)
print(Results_Age)
Results_BayesianMeta_Analysis = rbind(Results_Age)

Results_Comorbidity = BayesUpdateStepByStep(x, Construct = "Comorbidity", uncertainty = uncertainty, seed = seed)
print(Results_Comorbidity)
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_Comorbidity)

Results_SocialSupport = BayesUpdateStepByStep(x, Construct = "SocialSupport", uncertainty = uncertainty, seed = seed)
print(Results_SocialSupport)
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_SocialSupport)

Results_negative_attitude = BayesUpdateStepByStep(x, Construct = "NegativeAttitute", uncertainty = uncertainty, seed = seed)
print(Results_negative_attitude)
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_negative_attitude)

Results_positive_attitude = BayesUpdateStepByStep(x, Construct = "PositiveAttitute", uncertainty = uncertainty, seed = seed)
print(Results_positive_attitude)
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_positive_attitude)

Results_positive_Soma_6MWT= BayesUpdateStepByStep(x, Construct = "6MWT", uncertainty = uncertainty, seed = seed)
print(Results_positive_Soma_6MWT)
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_positive_Soma_6MWT)

Results_Soma_Functioning =  BayesUpdateStepByStep(x, Construct = "Functioning", uncertainty = uncertainty, seed = seed)
print(Results_Soma_Functioning)
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_Soma_Functioning)

Results_Symptoms_Dyspnoea = BayesUpdateStepByStep(x, Construct = "Symptoms", uncertainty = uncertainty, seed = seed)
print(Results_Symptoms_Dyspnoea)
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_Symptoms_Dyspnoea)

Results_Symptoms_Soma_EF= BayesUpdateStepByStep(x, Construct = "LVEF", uncertainty = uncertainty, seed = seed)
print(Results_Symptoms_Soma_EF)
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_Symptoms_Soma_EF)

Results_SelfEfficacy = BayesUpdateStepByStep(x = x, Construct ="SelfEfficacy", uncertainty = uncertainty, seed = seed)
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_SelfEfficacy)


print(Results_BayesianMeta_Analysis)

############################### saving csv file of the results for this seed and this uncertainty ############################
logName = as.character(uncertainty + seed)

file_x <- file.path("/Users/aliya/my_docs/proj/bayesian_meta_analysis", logName, "Results_BayesianMeta_Analysis.csv")
fn <- as.filename(file_x)
make_path(fn)
write.table(Results_BayesianMeta_Analysis, file = file_x, 
            append = FALSE, 
            quote = TRUE, 
            sep = ",", 
            eol = "\r", 
            na = "NA", 
            dec = ".",
            row.names = FALSE, 
            col.names = TRUE, 
            fileEncoding = "" )

#seed_string = read.csv(file_x) 
#print(seed_string)
mean_posterior_string_SEED = Results_BayesianMeta_Analysis$mean_posterior
print(mean_posterior_string_SEED)

Pooled_LOGOdds_Ratio_posterior_string_SEED = Results_BayesianMeta_Analysis$Pooled_LOGOdds_Ratio
print(Pooled_LOGOdds_Ratio_posterior_string_SEED)

Pooled_LOGOdds_Ratio_posterior_string_SEED = Results_BayesianMeta_Analysis$Pooled_LOGOdds_Ratio
print(Pooled_LOGOdds_Ratio_posterior_string_SEED)

LowerCI_LogOddsRatio_posterior_string_SEED = Results_BayesianMeta_Analysis$LowerCI_LogOddsRatio
UpperCI_LogOddsRatio_posterior_string_SEED = Results_BayesianMeta_Analysis$UpperCI_LogOddsRatio

posterior_alpha_Ratio_posterior_string_SEED = Results_BayesianMeta_Analysis$posterior_alpha
print(posterior_alpha_Ratio_posterior_string_SEED)

posterior_alpha_Ratio_posterior_string_SEED = Results_BayesianMeta_Analysis$posterior_alpha
print(posterior_alpha_Ratio_posterior_string_SEED)

posterior_beta_Ratio_posterior_string_SEED = Results_BayesianMeta_Analysis$posterior_beta
print(posterior_beta_Ratio_posterior_string_SEED)


posterior_CredibleInterval_0.05_string_SEED = Results_BayesianMeta_Analysis$posterior_CredibleInterval_0.05
print(posterior_CredibleInterval_0.05_string_SEED)


posterior_CredibleInterval_0.95_string_SEED = Results_BayesianMeta_Analysis$posterior_CredibleInterval_0.95
print(posterior_CredibleInterval_0.95_string_SEED)


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

x_directory <- file.path("/Users/aliya/my_docs/proj/bayesian_meta_analysis", logName)
file.copy(from=plots.png.paths, to=x_directory)



#################################################################################### count how many pair-wise analyses were continious-continious; continious-binary, continious categorical (etc)

Age_num = VarPairType(VarData = VarData, Construct = "Age")
Comorbidity_num = VarPairType(VarData = VarData, Construct = "Comorbidity")
SocialSupport_num = VarPairType(VarData = VarData, Construct = "SocialSupport")
NegativeAttitute = VarPairType(VarData = VarData, Construct = "NegativeAttitute")
PositiveAttitute = VarPairType(VarData = VarData, Construct = "PositiveAttitute")
SixMWT_num = VarPairType(VarData = VarData, Construct = "6MWT")
Functioning_num = VarPairType(VarData = VarData, Construct = "Functioning")
Symptoms_num = VarPairType(VarData = VarData, Construct = "Symptoms")
LVEF_num = VarPairType(VarData = VarData, Construct = "LVEF")
SelfEfficacy_num = VarPairType(VarData = VarData, Construct = "SelfEfficacy")


Construct_name = c("Age",
                   "Comorbidity",
                   "SocialSupport",
                   "NegativeAttitute",
                   "PositiveAttitute",
                   "6MWT",
                   "Functioning",
                   "Symptoms",
                   "LVEF",
                   "SelfEfficacy")

Number_ofStudies_PerComparison_MixedBayes = rbind(Age_num,
                                                  Comorbidity_num,
                                                  SocialSupport_num,
                                                  NegativeAttitute,
                                                  PositiveAttitute,
                                                  SixMWT_num,
                                                  Functioning_num,
                                                  Symptoms_num,
                                                  LVEF_num,
                                                  SelfEfficacy_num)

Comparison_MixedBayes  = cbind(Construct_name,Number_ofStudies_PerComparison_MixedBayes)

print(Comparison_MixedBayes)



file_x2 <- file.path("/Users/aliya/my_docs/proj/bayesian_meta_analysis", logName, "Comparison_MixedBayes.csv")
fn <- as.filename(file_x2)
make_path(fn)
write.table(Comparison_MixedBayes, file = file_x2, 
            append = FALSE, 
            quote = TRUE, 
            sep = ",", 
            eol = "\r", 
            na = "NA", 
            dec = ".",
            row.names = FALSE, 
            col.names = TRUE, 
            fileEncoding = "" )

print(mean_posterior_string_SEED)
