library(ggplot2)
library(ggridges)
library(dplyr)


#source('/Users/aliya/my_docs/proj/bayes_meta/Density_ggplot.R')

theme_set(theme_minimal())

data = read.csv(paste(OUTPUT_ROOT, "Quant_Mean_results_over_SEEDS_QUANTonly_FINAL_RESULTS.csv", sep=""))
data$Construct_name
density_by_cosntruct = function(data, Construct_name){
  index = data$Construct_name == Construct_name
  Theta = seq(0.01, 0.99, 0.01)
  filtered_data = filter(data, Construct_name == data[index,]$Construct_name)
  posterior_by_constructs = dbeta(Theta, filtered_data$posterior_alpha_mean_uncertaintyLevels_MAP,
                                  filtered_data$posterior_beta_mean_uncertaintyLevels_MAP)
  #posterior_by_constructs = exp(posterior_by_constructs)
  df = data.frame(Theta, Construct_name, posterior_by_constructs)
  colnames(df) = c("Theta", "construct", "posterior")
  return(df)
}


Age_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Age")
Comorbidity_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Comorbidity")
SocialSupport_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "SocialSupport")
NegativeAttitute_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "NegativeAttitute")
PositiveAttitute_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "PositiveAttitute")
SixMWT_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "6MWT")
Functioning_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Functioning")
Symptoms_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Symptoms")
LVEF_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "LVEF")
SelfEfficacy_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "SelfEfficacy")
#BloodPressure_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "BloodPressure")
#BMI_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "BMI")
#CRT_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "CRT")
Depression_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Depression")
Digoxin_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Digoxin")
Doppler_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Doppler")
Dysphoria_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Dysphoria")
Education_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Education")
Employment_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Employment")
Ethnicity_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Ethnicity")
#Functioning_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Functioning")
HFDuration_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "HFDuration")
HFrEF_Yes_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "HFrEF_Yes")
highproBNP_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "highproBNP")
Hostility_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Hostility")
Income_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Income")
LAV_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "LAV")
LVAD_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "LVAD")
LVR_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "LVR")
Partner_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Partner")
PeakVO2_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "PeakVO2")
PercievedExersion_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "PercievedExersion")
QoL_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "QoL")
RenalFunction_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "RenalFunction")
Smoking_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Smoking")
Symptoms_distress_density_by_cosntruct = density_by_cosntruct(data = data, Construct_name = "Symptoms_distress")



height = c(rep(1, 99),
  rep(2, 99), 
  rep(3, 99), 
  rep(4, 99), 
  rep(5, 99), 
  rep(6, 99), 
  rep(7, 99), 
  rep(8, 99), 
  rep(9, 99), 
  rep(10, 99), 
  rep(11, 99), 
  rep(12, 99), 
  rep(13, 99), 
  rep(14, 99), 
  rep(15, 99), 
  rep(16, 99), 
  rep(17, 99), 
  rep(18, 99), 
  rep(19, 99), 
  rep(20, 99), 
  rep(21, 99), 
  rep(22, 99), 
  rep(23, 99), 
  rep(24, 99), 
  rep(25, 99), 
  rep(26, 99), 
  rep(27, 99), 
  rep(28, 99), 
  rep(29, 99), 
  rep(30, 99), 
  rep(31, 99), 
  rep(32, 99))
  
length(height)
density_ALL_cosntruct = rbind(Age_density_by_cosntruct,
                                Comorbidity_density_by_cosntruct,
                                SocialSupport_density_by_cosntruct,
                                NegativeAttitute_density_by_cosntruct,
                                PositiveAttitute_density_by_cosntruct,
                                SixMWT_density_by_cosntruct, 
                                Functioning_density_by_cosntruct,
                                Symptoms_density_by_cosntruct,
                                LVEF_density_by_cosntruct, 
                                SelfEfficacy_density_by_cosntruct,
                                Depression_density_by_cosntruct,
                                Digoxin_density_by_cosntruct,
                                Doppler_density_by_cosntruct,
                                Dysphoria_density_by_cosntruct,
                                Education_density_by_cosntruct,
                                Employment_density_by_cosntruct,
                                Ethnicity_density_by_cosntruct,
                                #Functioning_density_by_cosntruct,
                                HFDuration_density_by_cosntruct,
                                HFrEF_Yes_density_by_cosntruct,
                                highproBNP_density_by_cosntruct,
                                Hostility_density_by_cosntruct,
                                Income_density_by_cosntruct,
                                LAV_density_by_cosntruct,
                                LVAD_density_by_cosntruct,
                                LVR_density_by_cosntruct,
                                Partner_density_by_cosntruct,
                                PeakVO2_density_by_cosntruct,
                                PercievedExersion_density_by_cosntruct,
                                QoL_density_by_cosntruct,
                                RenalFunction_density_by_cosntruct,
                                Smoking_density_by_cosntruct,
                                Symptoms_distress_density_by_cosntruct)

density_ALL_cosntruct = cbind(density_ALL_cosntruct, height)
nrow(density_ALL_cosntruct)
#density_ALL_cosntruct = rbind(density_ALL_cosntruct, Construct_name)

#graph_Posterior = plotDensity(data = Age_density_by_cosntruct,
#                            aes( x = Age_density_by_cosntruct$Theta, 
#                                  y = Age_density_by_cosntruct$posterior_by_constructs,
 #                                fill = "#00AFBB"))



#plot2 = ggplot(density_ALL_cosntruct, aes(y = posterior, x = Theta))+
#  geom_path(colour = "#00AFBB")+
 # xlim(0.05, 0.505)+
 # facet_wrap(~construct, ncol = 3)
#  facet_grid(rows = vars(construct))

Age_density_by_cosntruct$posterior

 # geom_density_ridges(scale = 1) + facet_wrap(~construct)
#print(plot2)

#plot = ggplot(density_ALL_cosntruct, aes(x = Theta, y = posterior, group = construct)) + 
# geom_density_ridges(fill = "#00AFBB")

#plot

#plot3 = ggplot(density_ALL_cosntruct, aes(x = posterior, y = construct)) + 
#  geom_density_ridges(rel_min_height = 0.01)

mean(density_ALL_cosntruct$posterior)

max(density_ALL_cosntruct$posterior)

min(density_ALL_cosntruct$posterior)

d <- data.frame(
  x = density_ALL_cosntruct$Theta, 
  y = c(Age_density_by_cosntruct$posterior,
         Comorbidity_density_by_cosntruct$posterior,
         SocialSupport_density_by_cosntruct$posterior,
         NegativeAttitute_density_by_cosntruct$posterior,
         PositiveAttitute_density_by_cosntruct$posterior,
         SixMWT_density_by_cosntruct$posterior, 
         Functioning_density_by_cosntruct$posterior,
         Symptoms_density_by_cosntruct$posterior,
         LVEF_density_by_cosntruct$posterior, 
         SelfEfficacy_density_by_cosntruct$posterior,
         Depression_density_by_cosntruct$posterior,
         Digoxin_density_by_cosntruct$posterior,
         Doppler_density_by_cosntruct$posterior,
         Dysphoria_density_by_cosntruct$posterior,
         Education_density_by_cosntruct$posterior,
         Employment_density_by_cosntruct$posterior,
         Ethnicity_density_by_cosntruct$posterior,
         #Functioning_density_by_cosntruct$posterior,
         HFDuration_density_by_cosntruct$posterior,
         HFrEF_Yes_density_by_cosntruct$posterior,
         highproBNP_density_by_cosntruct$posterior,
         Hostility_density_by_cosntruct$posterior,
         Income_density_by_cosntruct$posterior,
         LAV_density_by_cosntruct$posterior,
         LVAD_density_by_cosntruct$posterior,
         LVR_density_by_cosntruct$posterior,
         Partner_density_by_cosntruct$posterior,
         PeakVO2_density_by_cosntruct$posterior,
         PercievedExersion_density_by_cosntruct$posterior,
         QoL_density_by_cosntruct$posterior,
         RenalFunction_density_by_cosntruct$posterior,
         Smoking_density_by_cosntruct$posterior,
         Symptoms_distress_density_by_cosntruct$posterior),
  height = height)

plot5 = ggplot(d, aes(x, y, height = height, group = y)) + 
  geom_ridgeline(fill = "lightblue")

print(plot5)

plot4 = ggplot(density_ALL_cosntruct, aes(x = Theta, y = construct, height=posterior, group = construct)) +
  geom_density_ridges(stat = "identity", scale = 10) +
  xlim(0, 0.5)
      # +
  
 # ylim(c("Age",
 #"Comorbidity",
#"SocialSupport",
#"NegativeAttitute",
#"PositiveAttitute",
#"6MWT",
#"Functioning",
#"Symptoms",
#"LVEF",
#"SelfEfficacy",
 #"BloodPressure",
       #B"BMI",
       #"CRT",
#"Depression",
#"Digoxin",
#"Doppler",
#"Dysphoria",
# "Education",
#"Employment",
#"Ethnicity",
# "Functioning",
# "HFDuration",
# "HFrEF_Yes",
# "highproBNP",
#"Hostility",
# "Income",
#"LAV",
# "LVAD",
# "LVR",
#"Partner",
# "PeakVO2",
# "PercievedExersion",
#"QoL",
#"RenalFunction",
#"Smoking",
# "Symptoms distress"))


print(plot4)
