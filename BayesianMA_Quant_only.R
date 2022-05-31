

# this scripts performs Baesian meta-analysis of quantitative evidence separately for each construct 
# the empirical hyperprior is updated with the quantitative findings 

library(tidyverse)
library(dplyr)
library(assertthat)
library(ggplot2)
library(reshape2)  
library(filenamer) # library for as.filename
library(ggridges)



source(paste(SOURCE_ROOT, "BayesUpdate_Quant.R", sep="")) # function that runs Bayesian meta-analysis of quantitative evidence

source(paste(SOURCE_ROOT, "Summary_stats_table.R", sep="")) # function that runs Bayesian meta-analysis of quantitative evidence and outputs the summary stats only as opposed to the entire distribution 

## THE BAYES UPDATE WITHOUT THE PRIOR ELICITED FROM THE QUALITATIVE STUDIES
### Bayes update: Jaarsma's empirical hyperprior + quantitative studies 

ResultsBayesianUpdateQuant = data.frame()
Summary_statistics_table_quant_only = data.frame()

QuantUpdate_Age = BayesUpdate_Quant(data = data, Construct = "Age")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Age)

Summary_stats_table_Age = Summary_stats_table(data = data, Construct = "Age")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_Age)

QuantUpdate_Comorbidity = BayesUpdate_Quant(data = data, Construct = "Comorbidity")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Comorbidity)

Summary_stats_table_Comorbidity = Summary_stats_table(data = data, Construct = "Comorbidity")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_Comorbidity)


QuantUpdate_SocialSupport = BayesUpdate_Quant(data = data, Construct = "SocialSupport")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_SocialSupport)

Summary_stats_table_SocialSupport = Summary_stats_table(data = data, Construct = "SocialSupport")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_SocialSupport)

QuantUpdate_NegativeAttitude = BayesUpdate_Quant(data = data, Construct = "NegativeAttitude")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_NegativeAttitude)

Summary_stats_table_NegativeAttitude = Summary_stats_table(data = data, Construct = "NegativeAttitude")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_NegativeAttitude)

QuantUpdate_PositiveAttitude = BayesUpdate_Quant(data = data, Construct = "PositiveAttitude")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PositiveAttitude)


Summary_stats_table_PositiveAttitude= Summary_stats_table(data = data, Construct = "PositiveAttitude")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_PositiveAttitude)


QuantUpdate_6MWT= BayesUpdate_Quant(data = data, Construct = "6MWT")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_6MWT)



Summary_stats_table_6MWT= Summary_stats_table(data = data, Construct = "6MWT")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_6MWT)


QuantUpdate_PhysicalFunctioning= BayesUpdate_Quant(data = data, Construct = "PhysicalFunctioning")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PhysicalFunctioning)


Summary_stats_table_PhysicalFunctioning= Summary_stats_table(data = data, Construct = "PhysicalFunctioning")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_PhysicalFunctioning)

QuantUpdate_Symptoms= BayesUpdate_Quant(data = data, Construct = "Symptoms")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Symptoms)


Summary_stats_table_Symptoms = Summary_stats_table(data = data, Construct = "Symptoms")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_Symptoms)


QuantUpdate_LVEF = BayesUpdate_Quant(data = data, Construct = "LVEF")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LVEF)


Summary_stats_table_LVEF = Summary_stats_table(data = data, Construct = "LVEF")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_LVEF)

QuantUpdate_SelfEfficacy = BayesUpdate_Quant(data = data, Construct = "SelfEfficacy")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_SelfEfficacy)



Summary_stats_table_SelfEfficacy = Summary_stats_table(data = data, Construct = "SelfEfficacy")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_SelfEfficacy)



QuantUpdate_Depression = BayesUpdate_Quant(data = data, Construct = "Depression")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Depression)


Summary_stats_table_Depression = Summary_stats_table(data = data, Construct = "Depression")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_Depression)


QuantUpdate_Digoxin = BayesUpdate_Quant(data = data, Construct = "Digoxin")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Digoxin)


Summary_stats_table_Digoxin = Summary_stats_table(data = data, Construct = "Digoxin")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_Digoxin)


QuantUpdate_Doppler = BayesUpdate_Quant(data = data, Construct = "Doppler")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Doppler)


Summary_stats_table_Doppler = Summary_stats_table(data = data, Construct = "Doppler")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_Doppler)


QuantUpdate_Dysphoria = BayesUpdate_Quant(data = data, Construct = "Dysphoria")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Dysphoria)


Summary_stats_table_Dysphoria = Summary_stats_table(data = data, Construct = "Dysphoria")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_Dysphoria)


QuantUpdate_Employment = BayesUpdate_Quant(data = data, Construct = "Employment")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Employment)

Summary_stats_table_Employment = Summary_stats_table(data = data, Construct = "Employment")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_Employment)


QuantUpdate_Ethnicity = BayesUpdate_Quant(data = data, Construct = "Ethnicity")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Ethnicity)


Summary_stats_table_Ethnicity = Summary_stats_table(data = data, Construct = "Ethnicity")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_Ethnicity)


QuantUpdate_PhysicalFunctioning = BayesUpdate_Quant(data = data, Construct = "PhysicalFunctioning")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PhysicalFunctioning)



Summary_stats_table_PhysicalFunctioning = Summary_stats_table(data = data, Construct = "PhysicalFunctioning")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_PhysicalFunctioning)



QuantUpdate_HFDuration = BayesUpdate_Quant(data = data, Construct = "HFDuration")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_HFDuration)



Summary_stats_table_HFDuration = Summary_stats_table(data = data, Construct = "HFDuration")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_HFDuration)



QuantUpdate_HFrEF_Yes = BayesUpdate_Quant(data = data, Construct = "HFrEF_Yes")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_HFrEF_Yes)


Summary_stats_table_HFrEF_Yes = Summary_stats_table(data = data, Construct = "HFrEF_Yes")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_HFrEF_Yes)



QuantUpdate_highproBNP = BayesUpdate_Quant(data = data, Construct = "highproBNP")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_highproBNP)


Summary_stats_table_highproBNP = Summary_stats_table(data = data, Construct = "highproBNP")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_highproBNP)



QuantUpdate_Hostility = BayesUpdate_Quant(data = data, Construct = "Hostility")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Hostility)


Summary_stats_table_Hostility = Summary_stats_table(data = data, Construct = "Hostility")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_Hostility)



QuantUpdate_Income = BayesUpdate_Quant(data = data, Construct = "Income")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Income)



Summary_stats_table_Income = Summary_stats_table(data = data, Construct = "Income")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_Income)



QuantUpdate_LAV = BayesUpdate_Quant(data = data, Construct = "LAV")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LAV)



Summary_stats_table_LAV = Summary_stats_table(data = data, Construct = "LAV")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_LAV)


QuantUpdate_LVAD = BayesUpdate_Quant(data = data, Construct = "LVAD")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LVAD)



Summary_stats_table_LVAD = Summary_stats_table(data = data, Construct = "LVAD")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_LVAD)



QuantUpdate_LVR = BayesUpdate_Quant(data = data, Construct = "LVR")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_LVR)



Summary_stats_table_LVR= Summary_stats_table(data = data, Construct = "LVR")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_LVR)





QuantUpdate_Partner = BayesUpdate_Quant(data = data, Construct = "Partner")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Partner)


Summary_stats_table_Partner= Summary_stats_table(data = data, Construct = "Partner")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_Partner)




QuantUpdate_PeakVO2 = BayesUpdate_Quant(data = data, Construct = "PeakVO2")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PeakVO2)



Summary_stats_table_PeakVO2 = Summary_stats_table(data = data, Construct = "PeakVO2")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_PeakVO2)



QuantUpdate_PerceivedExertion = BayesUpdate_Quant(data = data, Construct = "PerceivedExertion")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_PerceivedExertion)


Summary_stats_table_PerceivedExertion = Summary_stats_table(data = data, Construct = "PerceivedExertion")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_PerceivedExertion)


QuantUpdate_QoL = BayesUpdate_Quant(data = data, Construct = "QoL")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_QoL)


Summary_stats_table_QoL = Summary_stats_table(data = data, Construct = "QoL")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_QoL)


QuantUpdate_RenalFunction = BayesUpdate_Quant(data = data, Construct = "RenalFunction")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_RenalFunction)


Summary_stats_table_RenalFunction = Summary_stats_table(data = data, Construct = "RenalFunction")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_RenalFunction)



QuantUpdate_Smoking = BayesUpdate_Quant(data = data, Construct = "Smoking")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Smoking)


Summary_stats_table_Smoking = Summary_stats_table(data = data, Construct = "Smoking")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_Smoking)



QuantUpdate_Symptoms_distress = BayesUpdate_Quant(data = data, Construct = "Symptoms_distress")
ResultsBayesianUpdateQuant = rbind(ResultsBayesianUpdateQuant, QuantUpdate_Symptoms_distress)


Summary_stats_table_Symptoms_distress= Summary_stats_table(data = data, Construct = "Symptoms_distress")
Summary_statistics_table_quant_only = rbind(Summary_statistics_table_quant_only, Summary_stats_table_Symptoms_distress)


write.table(ResultsBayesianUpdateQuant, file = paste(OUTPUT_ROOT, "Results_quant_only.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



write.table(Summary_statistics_table_quant_only, file = paste(OUTPUT_ROOT, "Results_Summary_statistics_table_quant_only.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
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
NegativeAttitude_density_by_Construct = density_by_Construct(data = data, Construct = "NegativeAttitude")
PositiveAttitude_density_by_Construct = density_by_Construct(data = data, Construct = "PositiveAttitude")
SixMWT_density_by_Construct = density_by_Construct(data = data, Construct = "6MWT")
PhysicalFunctioning_density_by_Construct = density_by_Construct(data = data, Construct = "PhysicalFunctioning")
Symptoms_density_by_Construct = density_by_Construct(data = data, Construct = "Symptoms")
LVEF_density_by_Construct = density_by_Construct(data = data, Construct = "LVEF")
SelfEfficacy_density_by_Construct = density_by_Construct(data = data, Construct = "SelfEfficacy")
Depression_density_by_Construct = density_by_Construct(data = data, Construct = "Depression")
Digoxin_density_by_Construct = density_by_Construct(data = data, Construct = "Digoxin")
Doppler_density_by_Construct = density_by_Construct(data = data, Construct = "Doppler")
Dysphoria_density_by_Construct = density_by_Construct(data = data, Construct = "Dysphoria")
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
PerceivedExertion_density_by_Construct = density_by_Construct(data = data, Construct = "PerceivedExertion")
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
density_ALL_Construct_quant_only = rbind(Age_density_by_Construct,
                                        Comorbidity_density_by_Construct,
                                        SocialSupport_density_by_Construct,
                                        NegativeAttitude_density_by_Construct,
                                        PositiveAttitude_density_by_Construct,
                                        SixMWT_density_by_Construct, 
                                        PhysicalFunctioning_density_by_Construct,
                                        Symptoms_density_by_Construct,
                                        LVEF_density_by_Construct, 
                                        SelfEfficacy_density_by_Construct,
                                        Depression_density_by_Construct,
                                        Digoxin_density_by_Construct,
                                        Doppler_density_by_Construct,
                                        Dysphoria_density_by_Construct,
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
                                        PerceivedExertion_density_by_Construct,
                                        QoL_density_by_Construct,
                                        RenalFunction_density_by_Construct,
                                        Smoking_density_by_Construct,
                                        Symptoms_distress_density_by_Construct)

density_ALL_Construct_quant_only = cbind(density_ALL_Construct_quant_only, height)


d <- data.frame(
  x = density_ALL_Construct_quant_only$logOddsRatio, 
  y = c(Age_density_by_Construct$posterior,
        Comorbidity_density_by_Construct$posterior,
        SocialSupport_density_by_Construct$posterior,
        NegativeAttitude_density_by_Construct$posterior,
        PositiveAttitude_density_by_Construct$posterior,
        SixMWT_density_by_Construct$posterior, 
        PhysicalFunctioning_density_by_Construct$posterior,
        Symptoms_density_by_Construct$posterior,
        LVEF_density_by_Construct$posterior, 
        SelfEfficacy_density_by_Construct$posterior,
        Depression_density_by_Construct$posterior,
        Digoxin_density_by_Construct$posterior,
        Doppler_density_by_Construct$posterior,
        Dysphoria_density_by_Construct$posterior,
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
        PerceivedExertion_density_by_Construct$posterior,
        QoL_density_by_Construct$posterior,
        RenalFunction_density_by_Construct$posterior,
        Smoking_density_by_Construct$posterior,
        Symptoms_distress_density_by_Construct$posterior),
  height = height)


#plot4 = ggplot(density_ALL_Construct_quant_only, aes(x = Theta, y = construct, height=posterior, group = construct)) +
#  geom_density_ridges(stat = "identity", scale = 10) +
#  xlim(0, 0.5)


#plotting likelihood (quantitative evidence only)
Plot_Likelihood = ggplot(density_ALL_Construct_quant_only, aes(x = logOddsRatio, y = Construct, height=Likelihood, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 3) +  geom_density_ridges(stat = "identity", scale = 3) +
  scale_x_continuous(name = "log OR", breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 2), limits = c(-2, 3)) +
  
  scale_y_discrete(labels=c("Age" =  "Age",
                            "Comorbidity" =  "Comorbidity",
                            "SocialSupport"=  "Social Support",
                            "NegativeAttitude"=  "Negative Attitude",
                            "PositiveAttitude"=  "Positive Attitude",
                            "6MWT"= "6MWT",
                            "PhysicalFunctioning"="Physical Functioning",
                            "Symptoms"= "Perceived Symptoms",
                            "LVEF"="LVEF",
                            "SelfEfficacy"="Self-efficacy",
                            "Depression"="Depression",
                            "Digoxin"= "Digoxin",
                            "Doppler"=  "Doppler",
                            "Dysphoria"="Dysphoria",
                            "Employment"=  "Employment",
                            "Ethnicity"=  "Ethnicity",
                            "HFDuration"=  "HF Duration",
                            "HFrEF_Yes"=  "HFrEF",
                            "highproBNP"="high proBNP",
                            "Hostility"= "Hostility",
                            "Income"= "Income",
                            "LAV"= "LAV",
                            "LVAD"=  "LVAD",
                            "LVR"=  "LVR",
                            "Partner"= "Partner",
                            "PeakVO2"= "Peak VO2",
                            "PerceivedExertion"= "Perceived Exertion",
                            "QoL"= "QoL",
                            "RenalFunction"=  "Renal Function",
                            "Smoking"=  "Smoking",
                            "Symptoms_distress"= "Symptoms Distress"))   + 
  
  
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.major = element_line(colour = "grey", size = 0.2)) +
        #panel.grid.minor = element_line(colour = "grey", size = 0.1)) +
  theme(text = element_text(size = 10))   

ggsave(file = paste(OUTPUT_ROOT, "/Plot_Likelihood.pdf",  sep=""),Plot_Likelihood, width=4, height=3, units="in", scale=3)

print(Plot_Likelihood)
#plotting the posterior resulted from updating prior with likelihood 
Plot_posterior_Quant = ggplot(density_ALL_Construct_quant_only, aes(x = logOddsRatio, y = Construct, height=posterior_Quant, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  
  scale_x_continuous(name = "log OR", breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 2), limits = c(-2, 3)) +

  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.major = element_line(colour = "grey", size = 0.2),
        panel.grid.minor = element_line(colour = "grey", size = 0.1)) +
  
  theme(text = element_text(size = 8))   

 print(Plot_posterior_Quant)
 
ggsave(file = paste(OUTPUT_ROOT, "/Plot_posterior_Quant.pdf",  sep=""),Plot_posterior_Quant, width=4, height=3, units="in", scale=3)
