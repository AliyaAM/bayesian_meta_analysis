

library(ggplot2)
library(ggridges)
library(dplyr)


#source('/Users/aliya/my_docs/proj/bayes_meta/Density_ggplot.R')

theme_set(theme_minimal())

data = read.csv(paste(OUTPUT_ROOT, "Mean_results_over_SEEDS_Qual_and_Quant_FINAL_RESULT.csv", sep=""))

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




height = c(rep(1, 99),
           rep(2, 99), 
           rep(3, 99), 
           rep(4, 99), 
           rep(5, 99), 
           rep(6, 99), 
           rep(7, 99), 
           rep(8, 99), 
           rep(9, 99),
           rep(9, 99))

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
                              SelfEfficacy_density_by_cosntruct)

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
        SelfEfficacy_density_by_cosntruct$posterior),
  height = height)

plot5 = ggplot(d, aes(x, y, height = height, group = y)) + 
  geom_ridgeline(fill = "lightblue")

print(plot5)

plot4 = ggplot(density_ALL_cosntruct, aes(x = Theta, y = construct, height=posterior, group = construct)) +
  geom_density_ridges(stat = "identity", scale = 2) +
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
