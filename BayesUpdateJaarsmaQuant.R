

# this scripts performs Baesian meta-analysis of quantitative evidence separately for each construct 
# the empirical hyperprior is updated with the quantitative findings 

library(tidyverse)
library(dplyr)
library(assertthat)
library(ggplot2)
library(reshape2)  



x = read.csv('/Users/aliya/my_docs/proj/bayesian_meta_analysis/input.csv')  #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 
data = read.csv('/Users/aliya/my_docs/proj/bayesian_meta_analysis/QuantData_CheckedForAccuracy_20March2020.csv')  #data extracted from from the quantitative studies 


source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/BayesianMetaAnalysis_QuantJaarsma.R') # function that runs Bayesian meta-analysis of quantitative evidence


## THE BAYES UPDATE WITHOUT THE PRIOR ELICITED FROM THE QUALITATIVE STUDIES
### Bayes update: Jaarsma's empirical hyperprior + quantitative studies 

ResultsBayesianUpdateQuant = data.frame()

QuantUpdate_Age = BayesUpdate_Quant(data = data, Construct = "Age", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Age)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Age)


QuantUpdate_Comorbidity = BayesUpdate_Quant(data = data, Construct = "Comorbidity" , uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Comorbidity)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Comorbidity)


QuantUpdate_SocialSupport = BayesUpdate_Quant(data = data, Construct = "SocialSupport", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_SocialSupport)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_SocialSupport)


QuantUpdate_NegativeAttitute = BayesUpdate_Quant(data = data, Construct = "NegativeAttitute", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_NegativeAttitute)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_NegativeAttitute)


QuantUpdate_PositiveAttitute = BayesUpdate_Quant(data = data, Construct = "PositiveAttitute", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_PositiveAttitute)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PositiveAttitute)


QuantUpdate_6MWT= BayesUpdate_Quant(data = data, Construct = "6MWT", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_6MWT)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_6MWT)


QuantUpdate_Functioning= BayesUpdate_Quant(data = data, Construct = "Functioning", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Functioning)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Functioning)


QuantUpdate_Symptoms= BayesUpdate_Quant(data = data, Construct = "Symptoms", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Symptoms)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Symptoms)


QuantUpdate_LVEF = BayesUpdate_Quant(data = data, Construct = "LVEF", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_LVEF)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LVEF)


QuantUpdate_SelfEfficacy = BayesUpdate_Quant(data = data, Construct = "SelfEfficacy", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_SelfEfficacy)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_SelfEfficacy)



QuantUpdate_Depression = BayesUpdate_Quant(data = data, Construct = "Depression", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Depression)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Depression)



QuantUpdate_Digoxin = BayesUpdate_Quant(data = data, Construct = "Digoxin", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Digoxin)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Digoxin)



QuantUpdate_Doppler = BayesUpdate_Quant(data = data, Construct = "Doppler", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Doppler)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Doppler)


QuantUpdate_Dysphoria = BayesUpdate_Quant(data = data, Construct = "Dysphoria", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Dysphoria)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Dysphoria)


QuantUpdate_Education = BayesUpdate_Quant(data = data, Construct = "Education", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Education)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Education)


QuantUpdate_Employment = BayesUpdate_Quant(data = data, Construct = "Employment", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Employment)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Employment)


QuantUpdate_Ethnicity = BayesUpdate_Quant(data = data, Construct = "Ethnicity", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Ethnicity)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Ethnicity)


QuantUpdate_Functioning = BayesUpdate_Quant(data = data, Construct = "Functioning", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Functioning)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Functioning)


QuantUpdate_HFDuration = BayesUpdate_Quant(data = data, Construct = "HFDuration", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_HFDuration)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_HFDuration)



QuantUpdate_HFrEF_Yes = BayesUpdate_Quant(data = data, Construct = "HFrEF_Yes", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_HFrEF_Yes)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_HFrEF_Yes)


QuantUpdate_highproBNP = BayesUpdate_Quant(data = data, Construct = "highproBNP", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_highproBNP)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_highproBNP)



QuantUpdate_Hostility = BayesUpdate_Quant(data = data, Construct = "Hostility", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Hostility)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Hostility)


QuantUpdate_Income = BayesUpdate_Quant(data = data, Construct = "Income", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Income)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Income)


QuantUpdate_LAV = BayesUpdate_Quant(data = data, Construct = "LAV", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_LAV)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LAV)


QuantUpdate_LVAD = BayesUpdate_Quant(data = data, Construct = "LVAD", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_LVAD)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LVAD)


QuantUpdate_LVR = BayesUpdate_Quant(data = data, Construct = "LVR", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_LVR)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LVR)



QuantUpdate_Partner = BayesUpdate_Quant(data = data, Construct = "Partner", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Partner)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Partner)


QuantUpdate_PeakVO2 = BayesUpdate_Quant(data = data, Construct = "PeakVO2", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_PeakVO2)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PeakVO2)


QuantUpdate_PercievedExersion = BayesUpdate_Quant(data = data, Construct = "PercievedExersion", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_PercievedExersion)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PercievedExersion)



QuantUpdate_QoL = BayesUpdate_Quant(data = data, Construct = "QoL", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_QoL)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_QoL)


QuantUpdate_RenalFunction = BayesUpdate_Quant(data = data, Construct = "RenalFunction", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_RenalFunction)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_RenalFunction)


QuantUpdate_Smoking = BayesUpdate_Quant(data = data, Construct = "Smoking", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Smoking)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Smoking)


QuantUpdate_Symptoms_distress = BayesUpdate_Quant(data = data, Construct = "Symptoms_distress", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Symptoms_distress)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Symptoms_distress)


print(ResultsBayesianUpdateQuant)
write.table(ResultsBayesianUpdateQuant, file = '/Users/aliya/my_docs/proj/ResultsBayesianUpdateQuant23Feb2021_QUANT.csv', append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )

print(likelihood_data)


logName = as.character(uncertainty + seed)

file_x <- file.path("/Users/aliya/my_docs/proj/bayesian_meta_analysis", logName, "Results_ResultsBayesianUpdateQuant.csv")
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

mean_posterior_string_SEED = ResultsBayesianUpdateQuant$mean_posterior
print(mean_posterior_string_SEED)

Pooled_LOGOdds_Ratio_posterior_string_SEED = ResultsBayesianUpdateQuant$Pooled_LOGOdds_Ratio
print(Pooled_LOGOdds_Ratio_posterior_string_SEED)

Pooled_LOGOdds_Ratio_posterior_string_SEED = ResultsBayesianUpdateQuant$Pooled_LOGOdds_Ratio
print(Pooled_LOGOdds_Ratio_posterior_string_SEED)

LowerCI_LogOddsRatio_posterior_string_SEED = ResultsBayesianUpdateQuant$LowerCI_LogOddsRatio
UpperCI_LogOddsRatio_posterior_string_SEED = ResultsBayesianUpdateQuant$UpperCI_LogOddsRatio

posterior_alpha_Ratio_posterior_string_SEED = ResultsBayesianUpdateQuant$posterior_alpha
print(posterior_alpha_Ratio_posterior_string_SEED)

posterior_alpha_Ratio_posterior_string_SEED = ResultsBayesianUpdateQuant$posterior_alpha
print(posterior_alpha_Ratio_posterior_string_SEED)

posterior_beta_Ratio_posterior_string_SEED = ResultsBayesianUpdateQuant$posterior_beta
print(posterior_beta_Ratio_posterior_string_SEED)


posterior_CredibleInterval_0.05_string_SEED = ResultsBayesianUpdateQuant$posterior_CredibleInterval_0.05
print(posterior_CredibleInterval_0.05_string_SEED)


posterior_CredibleInterval_0.95_string_SEED = ResultsBayesianUpdateQuant$posterior_CredibleInterval_0.95
print(posterior_CredibleInterval_0.95_string_SEED)


