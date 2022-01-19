#TESTING NEW BAYEs update fUNCTION 



## Set the root directory to look for source code.
SOURCE_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"

source(paste(SOURCE_ROOT, "BayesUpdateStepByStep.R", sep=""), local = TRUE) # this function (BayesUpdateStepByStep) runs the Bayesian meta-analysis that combines qualitative and quantitative evidence 

#source(paste(SOURCE_ROOT, "  BayesUpdateStepByStep.R", sep=""), local = TRUE)

#seed = as.integer(Sys.time())

Results_Age =   BayesUpdateStepByStep(x =x, Construct = "Age")
Results_BayesianMeta_Analysis = rbind(Results_Age)


Results_sixMWT =   BayesUpdateStepByStep(x =x, Construct = "6MWT")
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_sixMWT)

Results_Symptoms =   BayesUpdateStepByStep(x =x, Construct = "Symptoms"  )
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_Symptoms)


Results_LVEF =   BayesUpdateStepByStep(x =x, Construct = "LVEF"  )
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_LVEF)

Results_SelfEfficacy =   BayesUpdateStepByStep(x =x, Construct = "SelfEfficacy"  )
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_SelfEfficacy)

Results_SocialSupport =   BayesUpdateStepByStep(x =x, Construct = "SocialSupport"  )
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_SocialSupport)

Results_Comorbidity =   BayesUpdateStepByStep(x =x, Construct = "Comorbidity"  )
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_Comorbidity) 

Results_NegativeAttitute =   BayesUpdateStepByStep(x =x, Construct = "NegativeAttitute"  )
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_NegativeAttitute)

Results_Functioning =   BayesUpdateStepByStep(x =x, Construct = "Functioning"  )
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_Functioning)

Results_PositiveAttitute =   BayesUpdateStepByStep(x =x, Construct = "PositiveAttitute"  )
Results_BayesianMeta_Analysis = rbind(Results_BayesianMeta_Analysis, Results_PositiveAttitute)



density_by_construct = function(data, Construct){
  index = data$Construct == Construct
  logOddsRatio = seq( -2 , 2 , length=1000)
  filtered_data = filter(data, Construct == data[index,]$Construct)
  posterior_by_constructs = dnorm(logOddsRatio, filtered_data$posterior_All_mean,
                                  filtered_data$posterior_All_variance)
  #posterior_by_constructs = exp(posterior_by_constructs)
  df = data.frame(logOddsRatio, Construct, posterior_by_constructs)
  colnames(df) = c("logOddsRatio", "construct", "posterior")
  return(df)
}


Age_density_by_construct = density_by_construct(data = Results_BayesianMeta_Analysis, Construct = "Age")
Comorbidity_density_by_construct = density_by_construct(data = Results_BayesianMeta_Analysis, Construct = "Comorbidity")
SocialSupport_density_by_construct = density_by_construct(data = Results_BayesianMeta_Analysis, Construct = "SocialSupport")
NegativeAttitute_density_by_construct = density_by_construct(data = Results_BayesianMeta_Analysis, Construct = "NegativeAttitute")
PositiveAttitute_density_by_construct = density_by_construct(data = Results_BayesianMeta_Analysis, Construct = "PositiveAttitute")
SixMWT_density_by_construct = density_by_construct(data = Results_BayesianMeta_Analysis, Construct = "6MWT")
Functioning_density_by_construct = density_by_construct(data = Results_BayesianMeta_Analysis, Construct = "Functioning")
Symptoms_density_by_construct = density_by_construct(data = Results_BayesianMeta_Analysis, Construct = "Symptoms")
LVEF_density_by_construct = density_by_construct(data = Results_BayesianMeta_Analysis, Construct = "LVEF")
SelfEfficacy_density_by_construct = density_by_construct(data = Results_BayesianMeta_Analysis, Construct = "SelfEfficacy")




height = c(rep(1, 1000),
           rep(2, 1000), 
           rep(3, 1000), 
           rep(4, 1000), 
           rep(5, 1000), 
           rep(6, 1000), 
           rep(7, 1000), 
           rep(8, 1000), 
           rep(9, 1000),
           rep(9, 1000))

length(height)
density_ALL_cosntruct = rbind(Age_density_by_construct,
                              Comorbidity_density_by_construct,
                              SocialSupport_density_by_construct,
                              NegativeAttitute_density_by_construct,
                              PositiveAttitute_density_by_construct,
                              SixMWT_density_by_construct, 
                              Functioning_density_by_construct,
                              Symptoms_density_by_construct,
                              LVEF_density_by_construct, 
                              SelfEfficacy_density_by_construct)

density_ALL_cosntruct = cbind(density_ALL_cosntruct, height)
nrow(density_ALL_cosntruct)
#density_ALL_cosntruct = rbind(density_ALL_cosntruct, Construct)

#graph_Posterior = plotDensity(data = Age_density_by_construct,
#                            aes( x = Age_density_by_construct$logOddsRatio, 
#                                  y = Age_density_by_construct$posterior_by_constructs,
#                                fill = "#00AFBB"))



#plot2 = ggplot(density_ALL_cosntruct, aes(y = posterior, x = logOddsRatio))+
#  geom_path(colour = "#00AFBB")+
# xlim(0.05, 0.505)+
# facet_wrap(~construct, ncol = 3)
#  facet_grid(rows = vars(construct))

Age_density_by_construct$posterior

# geom_density_ridges(scale = 1) + facet_wrap(~construct)
#print(plot2)

#plot = ggplot(density_ALL_cosntruct, aes(x = logOddsRatio, y = posterior, group = construct)) + 
# geom_density_ridges(fill = "#00AFBB")

#plot

#plot3 = ggplot(density_ALL_cosntruct, aes(x = posterior, y = construct)) + 
#  geom_density_ridges(rel_min_height = 0.01)

mean(density_ALL_cosntruct$posterior)

max(density_ALL_cosntruct$posterior)

min(density_ALL_cosntruct$posterior)

d <- data.frame(
  x = density_ALL_cosntruct$logOddsRatio, 
  y = c(Age_density_by_construct$posterior,
        Comorbidity_density_by_construct$posterior,
        SocialSupport_density_by_construct$posterior,
        NegativeAttitute_density_by_construct$posterior,
        PositiveAttitute_density_by_construct$posterior,
        SixMWT_density_by_construct$posterior, 
        Functioning_density_by_construct$posterior,
        Symptoms_density_by_construct$posterior,
        LVEF_density_by_construct$posterior, 
        SelfEfficacy_density_by_construct$posterior),
  height = height)

plot5 = ggplot(d, aes(x, y, height = height, group = y)) + 
  geom_ridgeline(fill = "lightblue")

print(plot5)

plot4 = ggplot(density_ALL_cosntruct, aes(x = logOddsRatio, y = construct, height=posterior, group = construct)) +
  geom_density_ridges(stat = "identity", scale = 5) +
  xlim(-2, 2)
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
