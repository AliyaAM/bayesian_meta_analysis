

# this scripts performs Baesian meta-analysis of quantitative evidence separately for each construct 
# the empirical hyperprior is updated with the quantitative findings 

library(tidyverse)
library(dplyr)
library(assertthat)
library(ggplot2)
library(reshape2)  
library(filenamer) # library for as.filename
library(ggridges)


x = read.csv(paste(SOURCE_ROOT, "input.csv", sep=""))  #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 
data = read.csv(paste(SOURCE_ROOT, "QuantData_CheckedForAccuracy_20March2020.csv", sep=""))  #data extracted from from the quantitative studies 

source(paste(SOURCE_ROOT, "BayesUpdate_Quant.R", sep="")) # function that runs Bayesian meta-analysis of quantitative evidence


## THE BAYES UPDATE WITHOUT THE PRIOR ELICITED FROM THE QUALITATIVE STUDIES
### Bayes update: Jaarsma's empirical hyperprior + quantitative studies 

ResultsBayesianUpdateQuant = data.frame()

QuantUpdate_Age = BayesUpdate_Quant(data = data, Construct = "Age")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Age)


QuantUpdate_Comorbidity = BayesUpdate_Quant(data = data, Construct = "Comorbidity")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Comorbidity)


QuantUpdate_SocialSupport = BayesUpdate_Quant(data = data, Construct = "SocialSupport")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_SocialSupport)


QuantUpdate_NegativeAttitute = BayesUpdate_Quant(data = data, Construct = "NegativeAttitute")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_NegativeAttitute)


QuantUpdate_PositiveAttitute = BayesUpdate_Quant(data = data, Construct = "PositiveAttitute")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PositiveAttitute)


QuantUpdate_6MWT= BayesUpdate_Quant(data = data, Construct = "6MWT")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_6MWT)


QuantUpdate_Functioning= BayesUpdate_Quant(data = data, Construct = "Functioning")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Functioning)


QuantUpdate_Symptoms= BayesUpdate_Quant(data = data, Construct = "Symptoms")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Symptoms)


QuantUpdate_LVEF = BayesUpdate_Quant(data = data, Construct = "LVEF")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LVEF)


QuantUpdate_SelfEfficacy = BayesUpdate_Quant(data = data, Construct = "SelfEfficacy")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_SelfEfficacy)

QuantUpdate_Depression = BayesUpdate_Quant(data = data, Construct = "Depression")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Depression)


QuantUpdate_Digoxin = BayesUpdate_Quant(data = data, Construct = "Digoxin")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Digoxin)


QuantUpdate_Doppler = BayesUpdate_Quant(data = data, Construct = "Doppler", uncertainty = uncertainty, seed = seed)
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Doppler)


QuantUpdate_Dysphoria = BayesUpdate_Quant(data = data, Construct = "Dysphoria")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Dysphoria)


QuantUpdate_Education = BayesUpdate_Quant(data = data, Construct = "Education")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Education)


QuantUpdate_Employment = BayesUpdate_Quant(data = data, Construct = "Employment")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Employment)


QuantUpdate_Ethnicity = BayesUpdate_Quant(data = data, Construct = "Ethnicity")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Ethnicity)


QuantUpdate_Functioning = BayesUpdate_Quant(data = data, Construct = "Functioning")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Functioning)


QuantUpdate_HFDuration = BayesUpdate_Quant(data = data, Construct = "HFDuration")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_HFDuration)



QuantUpdate_HFrEF_Yes = BayesUpdate_Quant(data = data, Construct = "HFrEF_Yes")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_HFrEF_Yes)


QuantUpdate_highproBNP = BayesUpdate_Quant(data = data, Construct = "highproBNP")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_highproBNP)



QuantUpdate_Hostility = BayesUpdate_Quant(data = data, Construct = "Hostility")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Hostility)


QuantUpdate_Income = BayesUpdate_Quant(data = data, Construct = "Income")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Income)


QuantUpdate_LAV = BayesUpdate_Quant(data = data, Construct = "LAV")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LAV)


QuantUpdate_LVAD = BayesUpdate_Quant(data = data, Construct = "LVAD")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LVAD)


QuantUpdate_LVR = BayesUpdate_Quant(data = data, Construct = "LVR")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LVR)



QuantUpdate_Partner = BayesUpdate_Quant(data = data, Construct = "Partner")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Partner)


QuantUpdate_PeakVO2 = BayesUpdate_Quant(data = data, Construct = "PeakVO2")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PeakVO2)


QuantUpdate_PercievedExersion = BayesUpdate_Quant(data = data, Construct = "PercievedExersion")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PercievedExersion)



QuantUpdate_QoL = BayesUpdate_Quant(data = data, Construct = "QoL")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_QoL)


QuantUpdate_RenalFunction = BayesUpdate_Quant(data = data, Construct = "RenalFunction")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_RenalFunction)


QuantUpdate_Smoking = BayesUpdate_Quant(data = data, Construct = "Smoking")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Smoking)


QuantUpdate_Symptoms_distress = BayesUpdate_Quant(data = data, Construct = "Symptoms_distress")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Symptoms_distress)


write.table(ResultsBayesianUpdateQuant, file = paste(OUTPUT_ROOT, "Results_quant_only.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )


density_by_Construct = function(data, Construct){
  index = data$Construct == Construct
  logOddsRatio = seq( -2, 3 , length=1000)
  filtered_data = filter(data, Construct == data[index,]$Construct)
  
  
  # likelihood (quantitative evidence only)
  Likelihood = dnorm(logOddsRatio, 
                     filtered_data$LOGOdds_Ratio_quant, 
                     filtered_data$variance_quant)
  
  # the posterior resulted from updating hyperprior with likelihood 
  posterior_Quant = dnorm(logOddsRatio, 
                                  filtered_data$posterior_Quant_mean,
                                  filtered_data$posterior_Quant_variance)
  
  
  df = data.frame(logOddsRatio, Construct, Likelihood, posterior_Quant)
  colnames(df) = c("logOddsRatio", "Construct", "Likelihood", "posterior_Quant")
  return(df)
}

data = ResultsBayesianUpdateQuant

Age_density_by_Construct = density_by_Construct(data = data, Construct = "Age")
Comorbidity_density_by_Construct = density_by_Construct(data = data, Construct = "Comorbidity")
SocialSupport_density_by_Construct = density_by_Construct(data = data, Construct = "SocialSupport")
NegativeAttitute_density_by_Construct = density_by_Construct(data = data, Construct = "NegativeAttitute")
PositiveAttitute_density_by_Construct = density_by_Construct(data = data, Construct = "PositiveAttitute")
SixMWT_density_by_Construct = density_by_Construct(data = data, Construct = "6MWT")
Functioning_density_by_Construct = density_by_Construct(data = data, Construct = "Functioning")
Symptoms_density_by_Construct = density_by_Construct(data = data, Construct = "Symptoms")
LVEF_density_by_Construct = density_by_Construct(data = data, Construct = "LVEF")
SelfEfficacy_density_by_Construct = density_by_Construct(data = data, Construct = "SelfEfficacy")
Depression_density_by_Construct = density_by_Construct(data = data, Construct = "Depression")
Digoxin_density_by_Construct = density_by_Construct(data = data, Construct = "Digoxin")
Doppler_density_by_Construct = density_by_Construct(data = data, Construct = "Doppler")
Dysphoria_density_by_Construct = density_by_Construct(data = data, Construct = "Dysphoria")
Education_density_by_Construct = density_by_Construct(data = data, Construct = "Education")
Employment_density_by_Construct = density_by_Construct(data = data, Construct = "Employment")
Ethnicity_density_by_Construct = density_by_Construct(data = data, Construct = "Ethnicity")
HFDuration_density_by_Construct = density_by_Construct(data = data, Construct = "HFDuration")
HFrEF_Yes_density_by_Construct = density_by_Construct(data = data, Construct = "HFrEF_Yes")
highproBNP_density_by_Construct = density_by_Construct(data = data, Construct = "highproBNP")
Hostility_density_by_Construct = density_by_Construct(data = data, Construct = "Hostility")
Income_density_by_Construct = density_by_Construct(data = data, Construct = "Income")
LAV_density_by_Construct = density_by_Construct(data = data, Construct = "LAV")
LVAD_density_by_Construct = density_by_Construct(data = data, Construct = "LVAD")
LVR_density_by_Construct = density_by_Construct(data = data, Construct = "LVR")
Partner_density_by_Construct = density_by_Construct(data = data, Construct = "Partner")
PeakVO2_density_by_Construct = density_by_Construct(data = data, Construct = "PeakVO2")
PercievedExersion_density_by_Construct = density_by_Construct(data = data, Construct = "PercievedExersion")
QoL_density_by_Construct = density_by_Construct(data = data, Construct = "QoL")
RenalFunction_density_by_Construct = density_by_Construct(data = data, Construct = "RenalFunction")
Smoking_density_by_Construct = density_by_Construct(data = data, Construct = "Smoking")
Symptoms_distress_density_by_Construct = density_by_Construct(data = data, Construct = "Symptoms_distress")



height = c(rep(1, 1000),
           rep(2, 1000), 
           rep(3, 1000), 
           rep(4, 1000), 
           rep(5, 1000), 
           rep(6, 1000), 
           rep(7, 1000), 
           rep(8, 1000), 
           rep(9, 1000), 
           rep(10, 1000), 
           rep(11, 1000), 
           rep(12, 1000), 
           rep(13, 1000), 
           rep(14, 1000), 
           rep(15, 1000), 
           rep(16, 1000), 
           rep(17, 1000), 
           rep(18, 1000), 
           rep(19, 1000), 
           rep(20, 1000), 
           rep(21, 1000), 
           rep(22, 1000), 
           rep(23, 1000), 
           rep(24, 1000), 
           rep(25, 1000), 
           rep(26, 1000), 
           rep(27, 1000), 
           rep(28, 1000), 
           rep(29, 1000), 
           rep(30, 1000), 
           rep(31, 1000), 
           rep(32, 1000),
           rep(33, 1000))

length(height)
density_ALL_Construct = rbind(Age_density_by_Construct,
                              Comorbidity_density_by_Construct,
                              SocialSupport_density_by_Construct,
                              NegativeAttitute_density_by_Construct,
                              PositiveAttitute_density_by_Construct,
                              SixMWT_density_by_Construct, 
                              Functioning_density_by_Construct,
                              Symptoms_density_by_Construct,
                              LVEF_density_by_Construct, 
                              SelfEfficacy_density_by_Construct,
                              Depression_density_by_Construct,
                              Digoxin_density_by_Construct,
                              Doppler_density_by_Construct,
                              Dysphoria_density_by_Construct,
                              Education_density_by_Construct,
                              Employment_density_by_Construct,
                              Ethnicity_density_by_Construct,
                              HFDuration_density_by_Construct,
                              HFrEF_Yes_density_by_Construct,
                              highproBNP_density_by_Construct,
                              Hostility_density_by_Construct,
                              Income_density_by_Construct,
                              LAV_density_by_Construct,
                              LVAD_density_by_Construct,
                              LVR_density_by_Construct,
                              Partner_density_by_Construct,
                              PeakVO2_density_by_Construct,
                              PercievedExersion_density_by_Construct,
                              QoL_density_by_Construct,
                              RenalFunction_density_by_Construct,
                              Smoking_density_by_Construct,
                              Symptoms_distress_density_by_Construct)

density_ALL_Construct = cbind(density_ALL_Construct, height)


d <- data.frame(
  x = density_ALL_Construct$logOddsRatio, 
  y = c(Age_density_by_Construct$posterior,
        Comorbidity_density_by_Construct$posterior,
        SocialSupport_density_by_Construct$posterior,
        NegativeAttitute_density_by_Construct$posterior,
        PositiveAttitute_density_by_Construct$posterior,
        SixMWT_density_by_Construct$posterior, 
        Functioning_density_by_Construct$posterior,
        Symptoms_density_by_Construct$posterior,
        LVEF_density_by_Construct$posterior, 
        SelfEfficacy_density_by_Construct$posterior,
        Depression_density_by_Construct$posterior,
        Digoxin_density_by_Construct$posterior,
        Doppler_density_by_Construct$posterior,
        Dysphoria_density_by_Construct$posterior,
        Education_density_by_Construct$posterior,
        Employment_density_by_Construct$posterior,
        Ethnicity_density_by_Construct$posterior,
        HFDuration_density_by_Construct$posterior,
        HFrEF_Yes_density_by_Construct$posterior,
        highproBNP_density_by_Construct$posterior,
        Hostility_density_by_Construct$posterior,
        Income_density_by_Construct$posterior,
        LAV_density_by_Construct$posterior,
        LVAD_density_by_Construct$posterior,
        LVR_density_by_Construct$posterior,
        Partner_density_by_Construct$posterior,
        PeakVO2_density_by_Construct$posterior,
        PercievedExersion_density_by_Construct$posterior,
        QoL_density_by_Construct$posterior,
        RenalFunction_density_by_Construct$posterior,
        Smoking_density_by_Construct$posterior,
        Symptoms_distress_density_by_Construct$posterior),
  height = height)

plot5 = ggplot(d, aes(x, y, height = height, group = y)) + 
  geom_ridgeline(fill = "lightblue")

print(plot5)

plot4 = ggplot(density_ALL_Construct, aes(x = Theta, y = construct, height=posterior, group = construct)) +
  geom_density_ridges(stat = "identity", scale = 10) +
  xlim(0, 0.5)


#plotting likelihood (quantitative evidence only)
Plot_Likelihood = ggplot(density_ALL_Construct, aes(x = logOddsRatio, y = Construct, height=Likelihood, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-2, 3  )

print(Plot_Likelihood)

#plotting the posterior resulted from updating prior with likelihood 
#Plot_posterior_Quant = ggplot(density_ALL_Construct, aes(x = logOddsRatio, y = Construct, height=posterior_Quant, group = Construct)) +
 # geom_density_ridges(stat = "identity", scale = 1) +
#  xlim(-2, 3  )

#print(Plot_posterior_Quant)

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)


x_directory_quant <- file.path(paste(OUTPUT_ROOT, "/PLOTS_QUANT", sep=""))
dir.create(x_directory_quant)
file.copy(from=plots.png.paths, to=x_directory_quant)


