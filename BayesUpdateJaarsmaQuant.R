

# this scripts performs Baesian meta-analysis of quantitative evidence separately for each construct 
# the empirical hyperprior is updated with the quantitative findings 

library(tidyverse)
library(dplyr)
library(assertthat)
library(ggplot2)
library(reshape2)  
library(filenamer) # library for as.filename


x = read.csv(paste(SOURCE_ROOT, "input.csv", sep=""))  #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 
data = read.csv(paste(SOURCE_ROOT, "QuantData_CheckedForAccuracy_20March2020.csv", sep=""))  #data extracted from from the quantitative studies 

source(paste(SOURCE_ROOT, "BayesianMetaAnalysis_QuantJaarsma.R", sep="")) # function that runs Bayesian meta-analysis of quantitative evidence


## THE BAYES UPDATE WITHOUT THE PRIOR ELICITED FROM THE QUALITATIVE STUDIES
### Bayes update: Jaarsma's empirical hyperprior + quantitative studies 

ResultsBayesianUpdateQuant = data.frame()

QuantUpdate_Age = BayesUpdate_Quant(data = data, Construct = "Age", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Age)


QuantUpdate_Comorbidity = BayesUpdate_Quant(data = data, Construct = "Comorbidity" , uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Comorbidity)


QuantUpdate_SocialSupport = BayesUpdate_Quant(data = data, Construct = "SocialSupport", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_SocialSupport)


QuantUpdate_NegativeAttitute = BayesUpdate_Quant(data = data, Construct = "NegativeAttitute", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_NegativeAttitute)


QuantUpdate_PositiveAttitute = BayesUpdate_Quant(data = data, Construct = "PositiveAttitute", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PositiveAttitute)


QuantUpdate_6MWT= BayesUpdate_Quant(data = data, Construct = "6MWT", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_6MWT)


QuantUpdate_Functioning= BayesUpdate_Quant(data = data, Construct = "Functioning", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Functioning)


QuantUpdate_Symptoms= BayesUpdate_Quant(data = data, Construct = "Symptoms", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Symptoms)


QuantUpdate_LVEF = BayesUpdate_Quant(data = data, Construct = "LVEF", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LVEF)


QuantUpdate_SelfEfficacy = BayesUpdate_Quant(data = data, Construct = "SelfEfficacy", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_SelfEfficacy)

QuantUpdate_Depression = BayesUpdate_Quant(data = data, Construct = "Depression", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Depression)


QuantUpdate_Digoxin = BayesUpdate_Quant(data = data, Construct = "Digoxin", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Digoxin)


QuantUpdate_Doppler = BayesUpdate_Quant(data = data, Construct = "Doppler", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Doppler)


#QuantUpdate_Dysphoria = BayesUpdate_Quant(data = data, Construct = "Dysphoria", uncertainty = uncertainty, seed = seed)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Dysphoria)


QuantUpdate_Education = BayesUpdate_Quant(data = data, Construct = "Education", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Education)


QuantUpdate_Employment = BayesUpdate_Quant(data = data, Construct = "Employment", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Employment)


QuantUpdate_Ethnicity = BayesUpdate_Quant(data = data, Construct = "Ethnicity", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Ethnicity)


QuantUpdate_Functioning = BayesUpdate_Quant(data = data, Construct = "Functioning", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Functioning)


QuantUpdate_HFDuration = BayesUpdate_Quant(data = data, Construct = "HFDuration", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_HFDuration)



QuantUpdate_HFrEF_Yes = BayesUpdate_Quant(data = data, Construct = "HFrEF_Yes", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_HFrEF_Yes)


QuantUpdate_highproBNP = BayesUpdate_Quant(data = data, Construct = "highproBNP", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_highproBNP)



QuantUpdate_Hostility = BayesUpdate_Quant(data = data, Construct = "Hostility", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Hostility)


QuantUpdate_Income = BayesUpdate_Quant(data = data, Construct = "Income", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Income)


QuantUpdate_LAV = BayesUpdate_Quant(data = data, Construct = "LAV", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LAV)


QuantUpdate_LVAD = BayesUpdate_Quant(data = data, Construct = "LVAD", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LVAD)


QuantUpdate_LVR = BayesUpdate_Quant(data = data, Construct = "LVR", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LVR)



QuantUpdate_Partner = BayesUpdate_Quant(data = data, Construct = "Partner", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Partner)


#QuantUpdate_PeakVO2 = BayesUpdate_Quant(data = data, Construct = "PeakVO2", uncertainty = uncertainty, seed = seed)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PeakVO2)


QuantUpdate_PercievedExersion = BayesUpdate_Quant(data = data, Construct = "PercievedExersion", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PercievedExersion)



QuantUpdate_QoL = BayesUpdate_Quant(data = data, Construct = "QoL", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_QoL)


QuantUpdate_RenalFunction = BayesUpdate_Quant(data = data, Construct = "RenalFunction", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_RenalFunction)


QuantUpdate_Smoking = BayesUpdate_Quant(data = data, Construct = "Smoking", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Smoking)


QuantUpdate_Symptoms_distress = BayesUpdate_Quant(data = data, Construct = "Symptoms_distress", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Symptoms_distress)

# paste(SOURCE_ROOT, "BayesianMetaAnalysis_QuantJaarsma.R", sep="")
#paste(OUTPUT_ROOT, "ResultsBayesianUpdateQuant23Feb2021_QUANT.csv", sep="")

write.table(ResultsBayesianUpdateQuant, file = paste(OUTPUT_ROOT, "ResultsBayesianUpdateQuant23Feb2021_QUANT.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )

uncertainty = 10
logName = as.character(paste(uncertainty, seed, sep="__"))

file_x <- file.path(paste(OUTPUT_ROOT, logName, "/Results_ResultsBayesianUpdateQuant.csv", sep=""))
fn <- as.filename(file_x)
make_path(fn)
write.table(ResultsBayesianUpdateQuant, file = file_x, 
            append = FALSE, 
            quote = TRUE, 
            sep = ",", 
            eol = "\r", 
            na = "NA", 
            dec = ".",
            row.names = FALSE, 
            col.names = TRUE, 
            fileEncoding = "" )


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

# save the averaged over seeds results in the directory below: 
#x_directory <- file.path("/Users/aliya/my_docs/proj/bayesian_meta_analysis", logName)

x_directory_quant <- file.path(paste(OUTPUT_ROOT, logName, "/seed_MAP_PLOTS_QUANT", sep=""))
dir.create(x_directory_quant)
file.copy(from=plots.png.paths, to=x_directory_quant)

mean_posterior_string_SEED = ResultsBayesianUpdateQuant$mean_posterior
Pooled_LOGOdds_Ratio_posterior_string_SEED = ResultsBayesianUpdateQuant$Pooled_LOGOdds_Ratio
Pooled_LOGOdds_Ratio_posterior_string_SEED = ResultsBayesianUpdateQuant$Pooled_LOGOdds_Ratio
LowerCI_LogOddsRatio_posterior_string_SEED = ResultsBayesianUpdateQuant$LowerCI_LogOddsRatio
UpperCI_LogOddsRatio_posterior_string_SEED = ResultsBayesianUpdateQuant$UpperCI_LogOddsRatio
posterior_alpha_Ratio_posterior_string_SEED = ResultsBayesianUpdateQuant$posterior_alpha
posterior_alpha_Ratio_posterior_string_SEED = ResultsBayesianUpdateQuant$posterior_alpha
posterior_beta_Ratio_posterior_string_SEED = ResultsBayesianUpdateQuant$posterior_beta
posterior_CredibleInterval_0.05_string_SEED = ResultsBayesianUpdateQuant$posterior_CredibleInterval_0.05
posterior_CredibleInterval_0.95_string_SEED = ResultsBayesianUpdateQuant$posterior_CredibleInterval_0.95


