

data_compare_distributions = density_ALL_Construct

data_compare_distributions_age = subset(data_compare_distributions, data_compare_distributions$Construct == "Age")

data_compare_distributions_6MWT = subset(data_compare_distributions, data_compare_distributions$Construct == "6MWT")

data_compare_distributions_6MWT = subset(data_compare_distributions, data_compare_distributions$Construct == "Symptoms")

data_compare_distributions_6MWT = subset(data_compare_distributions, data_compare_distributions$Construct == "LVEF")

data_compare_distributions_6MWT = subset(data_compare_distributions, data_compare_distributions$Construct == "SelfEfficacy")

data_compare_distributions_6MWT = subset(data_compare_distributions, data_compare_distributions$Construct == "SocialSupport")

data_compare_distributions_6MWT = subset(data_compare_distributions, data_compare_distributions$Construct == "Comorbidity")

data_compare_distributions_6MWT = subset(data_compare_distributions, data_compare_distributions$Construct == "NegativeAttitude")

data_compare_distributions_6MWT = subset(data_compare_distributions, data_compare_distributions$Construct == "Functioning")

data_compare_distributions_6MWT = subset(data_compare_distributions, data_compare_distributions$Construct == "PositiveAttitude")



unique(data_compare_distributions$Construct)

########################################################################################################
########################################################################################################

#code for plotting quantitative evidence (prior), next to likelihood, and the posterior. 

#in the below table ombine Prior_qual_density, Likelihood, posterior_QualplusQuant and tag everything by the name of the construct and the type of the evidence (ie., prior, likelihood, posterior)

logOddsRatio = seq( -2.01, 3.01 , length=30000) 
# repeat 10000 x 3 below: 
distribution_all_constructs = c(data_compare_distributions$Prior_qual_density, data_compare_distributions$Likelihood, data_compare_distributions$posterior_QualplusQuant)

# a vector of 10000 x 3
Construct_vector = c(data_compare_distributions$Construct, data_compare_distributions$Construct, data_compare_distributions$Construct)

type_evidence = rep(c("Prior", "Likelihood", "Posterior"), each = 10000)


#"Age"              "6MWT"             "Symptoms"         "LVEF"             "SelfEfficacy"     "SocialSupport"    "Comorbidity"     
# "NegativeAttitude" "Functioning"      "PositiveAttitude"

d <- data.frame(x = logOddsRatio,
  distribution_all_constructs = distribution_all_constructs,
  Construct_vector = Construct_vector, 
  type_evidence = type_evidence)
                            
d$distribution_group<-paste0(as.character(d$Construct_vector)," ", as.character(d$type_evidence))

nrow(d)
plot4 = ggplot(d , aes(x = logOddsRatio, 
                       y = distribution_group,
                       height =   distribution_all_constructs, 
                       group = distribution_group, 
                       color = type_evidence, fill = type_evidence)) +
  
  geom_density_ridges(stat = "identity") +
  
  xlim(-3.0001, 3.000) +
  
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.grid.major = element_line(colour = "grey", size = 0.2),
        panel.grid.minor = element_line(colour = "grey", size = 0.1)) 


print(plot4)
