

library(ggplot2)
library(ggridges)
library(dplyr)


#source('/Users/aliya/my_docs/proj/bayes_meta/Density_ggplot.R')

theme_set(theme_minimal())

data_qual = read.csv("/Users/aliya/my_docs/proj/bayes_meta/Mean_results_over_SEEDS_23Feb2021.csv")

data_QUANT = read.csv("/Users/aliya/my_docs/proj/bayes_meta/QUANT_Mean_results_over_SEEDS_23Feb2021_QUANT.csv")
head(data)


head(data)

density_by_cosntruct = function(data, Construct_name, type_evidence){
  index = data$Construct_name == Construct_name
  Theta = seq(0.01, 0.99, 0.01)
  filtered_data = filter(data, Construct_name == data[index,]$Construct_name)
  posterior_by_constructs = dbeta(Theta, filtered_data$posterior_alpha_mean_uncertaintyLevels_MAP,
                                  filtered_data$posterior_beta_mean_uncertaintyLevels_MAP)
  #posterior_by_constructs = exp(posterior_by_constructs)
  df = data.frame(Theta, Construct_name, type_evidence, posterior_by_constructs)
  colnames(df) = c("Theta", "construct", "type_evidence", "posterior")
  return(df)
}





data_qual = subset(data_qual, select = -c(Construct_name) )


Construct_name = c("Age_qual_QUANT", 
                   "Comorbidity_qual_QUANT", 
                   "SocialSupport_qual_QUANT",
                   "NegativeAttitute_qual_QUANT", 
                   "PositiveAttitute_qual_QUANT",
                   "6MWT_qual_QUANT",
                   "Functioning_qual_QUANT",
                   "Symptoms_qual_QUANT",
                   "LVEF_qual_QUANT",
                   "SelfEfficacy_qual_QUANT")
  
data_qual = cbind(data_qual, Construct_name)


Age_density_by_cosntruct_qual = density_by_cosntruct(data = data_qual,
                                                     type_evidence ="qual + QUANT", 
                                                     Construct_name = "Age_qual_QUANT")

Comorbidity_density_by_cosntruct_qual = density_by_cosntruct(data = data_qual, 
                                                             type_evidence ="qual + QUANT",                                                              
                                                             Construct_name = "Comorbidity_qual_QUANT")

SocialSupport_density_by_cosntruct_qual = density_by_cosntruct(data = data_qual,
                                                               type_evidence ="qual + QUANT", 
                                                               Construct_name = "SocialSupport_qual_QUANT")

NegativeAttitute_density_by_cosntruct_qual = density_by_cosntruct(data = data_qual,
                                                                  type_evidence ="qual + QUANT", 
                                                                  Construct_name = "NegativeAttitute_qual_QUANT")

PositiveAttitute_density_by_cosntruct_qual = density_by_cosntruct(data = data_qual,
                                                                  type_evidence ="qual + QUANT", 
                                                                  Construct_name = "PositiveAttitute_qual_QUANT")

SixMWT_density_by_cosntruct_qual = density_by_cosntruct(data = data_qual,
                                                        type_evidence ="qual + QUANT", 
                                                        Construct_name = "6MWT_qual_QUANT")

Functioning_density_by_cosntruct_qual = density_by_cosntruct(data = data_qual, 
                                                             type_evidence ="qual + QUANT", 
                                                             Construct_name = "Functioning_qual_QUANT")

Symptoms_density_by_cosntruct_qual = density_by_cosntruct(data = data_qual, 
                                                          type_evidence ="qual + QUANT", 
                                                          Construct_name = "Symptoms_qual_QUANT")

LVEF_density_by_cosntruct_qual = density_by_cosntruct(data = data_qual, 
                                                      type_evidence ="qual + QUANT", 
                                                      Construct_name = "LVEF_qual_QUANT")

SelfEfficacy_density_by_cosntruct_qual = density_by_cosntruct(data = data_qual, 
                                                              type_evidence ="qual + QUANT", 
                                                              Construct_name = "SelfEfficacy_qual_QUANT")

#######################################################################################################
Age_density_by_cosntruct = density_by_cosntruct(data = data_QUANT, 
                                                type_evidence ="QUANT only",
                                                Construct_name = "Age")

Comorbidity_density_by_cosntruct = density_by_cosntruct(data = data_QUANT, 
                                                        type_evidence ="QUANT only",
                                                        Construct_name = "Comorbidity")

SocialSupport_density_by_cosntruct = density_by_cosntruct(data = data_QUANT,
                                                          type_evidence ="QUANT only",
                                                          Construct_name = "SocialSupport")

NegativeAttitute_density_by_cosntruct = density_by_cosntruct(data = data_QUANT, 
                                                             type_evidence ="QUANT only",
                                                             Construct_name = "NegativeAttitute")

PositiveAttitute_density_by_cosntruct = density_by_cosntruct(data = data_QUANT, 
                                                             type_evidence ="QUANT only",
                                                             Construct_name = "PositiveAttitute")

SixMWT_density_by_cosntruct = density_by_cosntruct(data = data_QUANT,
                                                   type_evidence ="QUANT only",
                                                   Construct_name = "6MWT")

Functioning_density_by_cosntruct = density_by_cosntruct(data = data_QUANT, 
                                                        type_evidence ="QUANT only",
                                                        Construct_name = "Functioning")

Symptoms_density_by_cosntruct = density_by_cosntruct(data = data_QUANT, 
                                                     type_evidence ="QUANT only",
                                                     Construct_name = "Symptoms")

LVEF_density_by_cosntruct = density_by_cosntruct(data = data_QUANT,
                                                 type_evidence ="QUANT only",
                                                 Construct_name = "LVEF")

SelfEfficacy_density_by_cosntruct = density_by_cosntruct(data = data_QUANT,
                                                         type_evidence ="QUANT only",
                                                         Construct_name = "SelfEfficacy")




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
           rep(20, 99))

length(height)
density_ALL_cosntruct = rbind(Age_density_by_cosntruct_qual,
                              Age_density_by_cosntruct,
                              Comorbidity_density_by_cosntruct_qual,
                              Comorbidity_density_by_cosntruct,
                              SocialSupport_density_by_cosntruct_qual,
                              SocialSupport_density_by_cosntruct,
                              NegativeAttitute_density_by_cosntruct_qual,
                              NegativeAttitute_density_by_cosntruct,
                              PositiveAttitute_density_by_cosntruct_qual,
                              PositiveAttitute_density_by_cosntruct,
                              SixMWT_density_by_cosntruct_qual, 
                              SixMWT_density_by_cosntruct,
                              Functioning_density_by_cosntruct_qual,
                              Functioning_density_by_cosntruct,
                              Symptoms_density_by_cosntruct_qual,
                              Symptoms_density_by_cosntruct,
                              LVEF_density_by_cosntruct_qual, 
                              LVEF_density_by_cosntruct,
                              SelfEfficacy_density_by_cosntruct_qual,
                              SelfEfficacy_density_by_cosntruct)

density_ALL_cosntruct = cbind(density_ALL_cosntruct, height)
nrow(density_ALL_cosntruct)
#density_ALL_cosntruct = rbind(density_ALL_cosntruct, Construct_name)

#graph_Posterior = plotDensity(data = Age_density_by_cosntruct_qual,
#                            aes( x = Age_density_by_cosntruct_qual$Theta, 
#                                  y = Age_density_by_cosntruct_qual$posterior_by_constructs,
#                                fill = "#00AFBB"))



#plot2 = ggplot(density_ALL_cosntruct, aes(y = posterior, x = Theta))+
#  geom_path(colour = "#00AFBB")+
# xlim(0.05, 0.505)+
# facet_wrap(~construct, ncol = 3)
#  facet_grid(rows = vars(construct))

Age_density_by_cosntruct_qual$posterior

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
  y = c(Age_density_by_cosntruct_qual$posterior,
        Age_density_by_cosntruct$posterior,
        
        Comorbidity_density_by_cosntruct_qual$posterior,
        Comorbidity_density_by_cosntruct$posterior,
 
        SocialSupport_density_by_cosntruct_qual$posterior,
        SocialSupport_density_by_cosntruct$posterior,
 
        NegativeAttitute_density_by_cosntruct_qual$posterior,
        NegativeAttitute_density_by_cosntruct$posterior,

        PositiveAttitute_density_by_cosntruct_qual$posterior,
        PositiveAttitute_density_by_cosntruct$posterior,

        SixMWT_density_by_cosntruct_qual$posterior, 
        SixMWT_density_by_cosntruct$posterior, 

        Functioning_density_by_cosntruct_qual$posterior,
        Functioning_density_by_cosntruct$posterior,

        Symptoms_density_by_cosntruct_qual$posterior,
        Symptoms_density_by_cosntruct$posterior,

        LVEF_density_by_cosntruct_qual$posterior, 
        LVEF_density_by_cosntruct$posterior, 

        SelfEfficacy_density_by_cosntruct_qual$posterior,
        SelfEfficacy_density_by_cosntruct$posterior),
  height = height)

#plot5 = ggplot(d, aes(x, y, height = height, group = y)) + 
#  geom_ridgeline(fill = "lightblue")

#print(plot5)

plot4 = ggplot(density_ALL_cosntruct, aes(x = Theta, 
                                          y = construct,
                                          height=posterior, 
                                          group = construct, 
                                          color = type_evidence,
                                          fill = type_evidence)) +
  
  geom_density_ridges(stat = "identity",
                      scale = 5) +
 
   xlim(0, 0.5) +
  
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.grid.major = element_line(colour = "grey", size = 0.2),
        panel.grid.minor = element_line(colour = "grey", size = 0.1)) 
#+
#scale_fill_cyclical(labels = c("Age" = "Age_qual_QUANT", 
#                               "Comorbidity"="Comorbidity_qual_QUANT", 
#                               "SocialSupport"="SocialSupport_qual_QUANT",
#                               "NegativeAttitute" ="NegativeAttitute_qual_QUANT", 
#                               "PositiveAttitute" = "PositiveAttitute_qual_QUANT",
#                               "6MWT"="6MWT_qual_QUANT",
#                               "Functioning"="Functioning_qual_QUANT",
#                               "Symptoms"= "Symptoms_qual_QUANT",
#                               "LVEF"="LVEF_qual_QUANT",
#                               "SelfEfficacy"="SelfEfficacy_qual_QUANT"), 
                    
#                    values = c("#ff0000", "#0000ff", "#ff8080", "#8080ff"))

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