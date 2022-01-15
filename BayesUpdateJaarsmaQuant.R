###################################### THE BAYES UPDATE WITHOUT THE QUALI STUDIES ###########
library(tidyverse)
library(dplyr)
library(assertthat)
library(ggplot2)
library(reshape2)  




data = read.csv('/Users/aliya/my_docs/proj/bayesian_meta_analysis/QuantData_CheckedForAccuracy_20March2020.csv') 
print(data)


source('/Users/aliya/my_docs/proj/bayesian_meta_analysis/BayesianMetaAnalysis_QuantJaarsma.R')


###################################### THE BAYES UPDATE WITHOUT THE QUALI STUDIES ###########
########################################################################### Bayes update Jaarsma Hyper prior + quant studies 

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


#QuantUpdate_Betablockers = BayesUpdate_Quant(data = data, Construct = "Betablockers")
#print(QuantUpdate_Betablockers)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Betablockers)

#error in BloodPressure: Error in if ((abs((samples_Contingencies[1, j] + samples_Contingencies[2,  : 
#missing value where TRUE/FALSE needed
#In addition: There were 50 or more warnings (use warnings() to see the first 50)
#QuantUpdate_BloodPressure = BayesUpdate_Quant(data = data, Construct = "BloodPressure", uncertainty = uncertainty, seed = seed)
#print(QuantUpdate_BloodPressure)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_BloodPressure)

#error in BMI: Error in if ((abs((samples_Contingencies[1, j] + samples_Contingencies[2,  : 
#missing value where TRUE/FALSE needed
#In addition: There were 50 or more warnings (use warnings() to see the first 50)
#QuantUpdate_BMI = BayesUpdate_Quant(data = data, Construct = "BMI", uncertainty = uncertainty, seed = seed)
#print(QuantUpdate_BMI)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_BMI)


#QuantUpdate_CABG = BayesUpdate_Quant(data = data, Construct = "CABG")
#print(QuantUpdate_CABG)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_CABG)


#QuantUpdate_CardiologyClinic= BayesUpdate_Quant(data = data, Construct = "CardiologyClinic")
#print(QuantUpdate_CardiologyClinic)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_CardiologyClinic)

#QuantUpdate_CD = BayesUpdate_Quant(data = data, Construct = "CD")
#print(QuantUpdate_CD)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_CD)

#QuantUpdate_COPD = BayesUpdate_Quant(data = data, Construct = "COPD")
#print(QuantUpdate_COPD)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_COPD)

#QuantUpdate_CRT = BayesUpdate_Quant(data = data, Construct = "CRT", uncertainty = uncertainty, seed = seed)
#print(QuantUpdate_CRT)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_CRT)


#QuantUpdate_Decision= BayesUpdate_Quant(data = data, Construct = "Decision")
#print(QuantUpdate_Decision)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Decision)


QuantUpdate_Depression = BayesUpdate_Quant(data = data, Construct = "Depression", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Depression)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Depression)


#QuantUpdate_Diabetes = BayesUpdate_Quant(data = data, Construct = "Diabetes")
#print(QuantUpdate_Diabetes)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Diabetes)


#QuantUpdate_DietBenefit = BayesUpdate_Quant(data = data, Construct = "DietBenefit")
#print(QuantUpdate_DietBenefit)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_DietBenefit)


QuantUpdate_Digoxin = BayesUpdate_Quant(data = data, Construct = "Digoxin", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Digoxin)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Digoxin)


#QuantUpdate_Diuretics = BayesUpdate_Quant(data = data, Construct = "Diuretics")
#print(QuantUpdate_Diuretics)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Diuretics)


QuantUpdate_Doppler = BayesUpdate_Quant(data = data, Construct = "Doppler", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Doppler)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Doppler)


QuantUpdate_Dysphoria = BayesUpdate_Quant(data = data, Construct = "Dysphoria", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Dysphoria)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Dysphoria)


QuantUpdate_Education = BayesUpdate_Quant(data = data, Construct = "Education", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Education)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Education)


#QuantUpdate_EmotionalScore = BayesUpdate_Quant(data = data, Construct = "EmotionalScore")
#print(QuantUpdate_EmotionalScore)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_EmotionalScore)


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


#QuantUpdate_HFpEF = BayesUpdate_Quant(data = data, Construct = "HFpEF")
#print(QuantUpdate_HFpEF)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_HFpEF)


#QuantUpdate_HFprogram = BayesUpdate_Quant(data = data, Construct = "HFprogram")
#print(QuantUpdate_HFprogram)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_HFprogram)


QuantUpdate_HFrEF_Yes = BayesUpdate_Quant(data = data, Construct = "HFrEF_Yes", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_HFrEF_Yes)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_HFrEF_Yes)


QuantUpdate_highproBNP = BayesUpdate_Quant(data = data, Construct = "highproBNP", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_highproBNP)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_highproBNP)


#QuantUpdate_Hospitalisaiton= BayesUpdate_Quant(data = data, Construct = "Hospitalisaiton")
#print(QuantUpdate_Hospitalisaiton)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Hospitalisaiton)


QuantUpdate_Hostility = BayesUpdate_Quant(data = data, Construct = "Hostility", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Hostility)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Hostility)


#QuantUpdate_Hypertension = BayesUpdate_Quant(data = data, Construct = "Hypertension")
#print(QuantUpdate_Hypertension)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Hypertension)


QuantUpdate_Income = BayesUpdate_Quant(data = data, Construct = "Income", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Income)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Income)


#QuantUpdate_Knowledge = BayesUpdate_Quant(data = data, Construct = "Knowledge", uncertainty = uncertainty, seed = seed)
#print(QuantUpdate_Knowledge)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Knowledge)


QuantUpdate_LAV = BayesUpdate_Quant(data = data, Construct = "LAV", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_LAV)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LAV)


QuantUpdate_LVAD = BayesUpdate_Quant(data = data, Construct = "LVAD", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_LVAD)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LVAD)


QuantUpdate_LVR = BayesUpdate_Quant(data = data, Construct = "LVR", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_LVR)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LVR)


#QuantUpdate_Male = BayesUpdate_Quant(data = data, Construct = "Male")
#print(QuantUpdate_Male)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Male)


#QuantUpdate_MedicineBenefit = BayesUpdate_Quant(data = data, Construct = "MedicineBenefit")
#print(QuantUpdate_MedicineBenefit)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_MedicineBenefit)


#QuantUpdate_Mentalhealth = BayesUpdate_Quant(data = data, Construct = "Mentalhealth")
#print(QuantUpdate_Mentalhealth)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Mentalhealth)


#QuantUpdate_MI = BayesUpdate_Quant(data = data, Construct = "MI")
#print(QuantUpdate_MI)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_MI)


#QuantUpdate_Motivation = BayesUpdate_Quant(data = data, Construct = "Motivation")
#print(QuantUpdate_Motivation)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Motivation)


#QuantUpdate_Neuroticism = BayesUpdate_Quant(data = data, Construct = "Neuroticism")
#print(QuantUpdate_Neuroticism)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Neuroticism)


#QuantUpdate_Number_ofIRERs = BayesUpdate_Quant(data = data, Construct = "Number_ofIRERs")
#print(QuantUpdate_Number_ofIRERs)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Number_ofIRERs)


#QuantUpdate_NYHA = BayesUpdate_Quant(data = data, Construct = "NYHA")
#print(QuantUpdate_NYHA)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_NYHA)


#QuantUpdate_NYHA_II = BayesUpdate_Quant(data = data, Construct = "NYHA_II")
#print(QuantUpdate_NYHA_II)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_NYHA_II)


#QuantUpdate_NYHA_IIIvsII = BayesUpdate_Quant(data = data, Construct = "NYHA_IIIvsII")
#print(QuantUpdate_NYHA_IIIvsII)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_NYHA_IIIvsII)


#QuantUpdate_Orthpea = BayesUpdate_Quant(data = data, Construct = "Orthpea")
#print(QuantUpdate_Orthpea)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Orthpea)


QuantUpdate_Partner = BayesUpdate_Quant(data = data, Construct = "Partner", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Partner)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Partner)


QuantUpdate_PeakVO2 = BayesUpdate_Quant(data = data, Construct = "PeakVO2", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_PeakVO2)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PeakVO2)


QuantUpdate_PercievedExersion = BayesUpdate_Quant(data = data, Construct = "PercievedExersion", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_PercievedExersion)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PercievedExersion)


#QuantUpdate_PrevCRT = BayesUpdate_Quant(data = data, Construct = "PrevCRT")
#print(QuantUpdate_PrevCRT)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PrevCRT)


#QuantUpdate_PreviousExercisetask = BayesUpdate_Quant(data = data, Construct = "PreviousExercisetask")
#print(QuantUpdate_PreviousExercisetask)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PreviousExercisetask)


#QuantUpdate_proBNP = BayesUpdate_Quant(data = data, Construct = "proBNP")
#print(QuantUpdate_proBNP)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_proBNP)



QuantUpdate_QoL = BayesUpdate_Quant(data = data, Construct = "QoL", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_QoL)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_QoL)


QuantUpdate_RenalFunction = BayesUpdate_Quant(data = data, Construct = "RenalFunction", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_RenalFunction)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_RenalFunction)


#QuantUpdate_Sex = BayesUpdate_Quant(data = data, Construct = "Sex")
#print(QuantUpdate_Sex)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Sex)


#QuantUpdate_Skill = BayesUpdate_Quant(data = data, Construct = "Skill")
#print(QuantUpdate_Skill)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Skill)


#QuantUpdate_SleepOpnea = BayesUpdate_Quant(data = data, Construct = "SleepOpnea")
#print(QuantUpdate_SleepOpnea)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_SleepOpnea)


QuantUpdate_Smoking = BayesUpdate_Quant(data = data, Construct = "Smoking", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Smoking)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Smoking)


QuantUpdate_Symptoms_distress = BayesUpdate_Quant(data = data, Construct = "Symptoms_distress", uncertainty = uncertainty, seed = seed)
print(QuantUpdate_Symptoms_distress)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Symptoms_distress)


#QuantUpdate_UnknownFactors = BayesUpdate_Quant(data = data, Construct = "UnknownFactors")
#print(QuantUpdate_UnknownFactors)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_UnknownFactors)


#QuantUpdate_VenusPressure = BayesUpdate_Quant(data = data, Construct = "VenusPressure")
#print(QuantUpdate_VenusPressure)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_VenusPressure)


#QuantUpdate_WalkingAids = BayesUpdate_Quant(data = data, Construct = "WalkingAids")
#print(QuantUpdate_WalkingAids)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_WalkingAids)


#QuantUpdate_SPYH = BayesUpdate_Quant(data = data, Construct = "SPYH")
#print(QuantUpdate_SPYH)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_SPYH)


#QuantUpdate_Ischemic = BayesUpdate_Quant(data = data, Construct = "Ischemic")
#print(QuantUpdate_Ischemic)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Ischemic)


#QuantUpdate_Nonischemic = BayesUpdate_Quant(data = data, Construct = "Nonischemic")
#print(QuantUpdate_Nonischemic)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Nonischemic)


#QuantUpdate_Diuretics = BayesUpdate_Quant(data = data, Construct = "Diuretics")
#print(QuantUpdate_Diuretics)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Diuretics)



#QuantUpdate_AceIhib = BayesUpdate_Quant(data = data, Construct = "AceIhib")
#print(QuantUpdate_AceIhib)
#ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_AceIhib)




print(ResultsBayesianUpdateQuant)
write.table(ResultsBayesianUpdateQuant, file = '/Users/aliya/my_docs/proj/ResultsBayesianUpdateQuant23Feb2021.csv', append = FALSE, quote = TRUE, sep = ", ",
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

#seed_string = read.csv(file_x) 
#print(seed_string)

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


#plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
#plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

#x_directory <- file.path("/Users/aliya/my_docs/proj/", logName)
#file.copy(from=plots.png.paths, to=x_directory)


############################## Errors commented above and described below: 

print("####################################################################      Errors in the data input    #####################################")


print("multivariate beta cannot be integrated into the pooled ES CABG is using multivariatebeta stat (Alosco et al., 2012")

print("the estimates for:  

      Betablockers,
      CardiologyClinic, 
      CD, 
      COPD, 
      Decision, 
      Diabetes, 
      DietBenefit,  
      Diuretics,
      HFpEF, 
      HFprogram,
      Hospitalisaiton,
      Hypertension,
      MedicineBenefit,
      Mentalhealth, 
      MI, 
      Motivation 
      Neuroticism
      Number_ofIRERs
      NYHA
      NYHA_II
      NYHA_IIIvsII
      Orthpea
      PrevCRT
      PreviousExercisetask
      Sex
      Skill
      SleepOpnea
      UnknownFactors
      VenusPressure
      SPYH
      Nonischemic
      Diuretics

      are not transformed from estimate value to log odds ratio in convert effect sizes or otherwise do not come out of the pipeline")

print("The MCMC for: 

      the emotional score,
      male
      proBNP
      PreviousExercisetask
      WalkingAid


      do not work (the contingencies table)  Error in if ((abs((samples_Contingencies[1, j] + samples_Contingencies[2,: 
      missing value where TRUE/FALSE needed
      In addition: There were 50 or more warnings (use warnings() to see the first 50)
      >")

